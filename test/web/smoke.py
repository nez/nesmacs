#!/usr/bin/env python3
"""Exercise the packaged web client in Chrome against the real editor in a PTY."""

import json
import os
import shutil
import signal
import socket
import subprocess
import sys
import tempfile
import time
import urllib.request
from pathlib import Path

import websocket

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "pty"))
from editor_pty import poll_exit, read_chunk, start, stop
CHROME = os.environ.get("NESMACS_CHROME") or next(
    (path for name in ("google-chrome", "chromium", "chromium-browser")
     if (path := shutil.which(name))),
    None,
)


def wait_for(predicate, label, timeout=12):
    end = time.monotonic() + timeout
    while time.monotonic() < end:
        if result := predicate():
            return result
        time.sleep(0.05)
    raise AssertionError(label)


class Editor:
    def __init__(self, directory):
        self.home = directory / "home"
        self.config = self.home / ".nesmacs"
        self.config.mkdir(parents=True)
        self.log = self.config / "debug.log"
        with socket.socket() as server:
            server.bind(("127.0.0.1", 0))
            self.port = server.getsockname()[1]
        self.url = f"http://127.0.0.1:{self.port}"
        self.pid = None
        self.terminal_output = b""

    def start(self):
        old_size = self.log.stat().st_size if self.log.exists() else 0
        self.pid, self.master = start(self.home,
                                      env={"NESMACS_WEB_PORT": str(self.port)},
                                      jar=os.environ.get("NESMACS_WEB_JAR"))

        def ready():
            self.drain()
            if poll_exit(self.pid) is not None:
                os.close(self.master)
                self.pid = None
                raise AssertionError("editor exited during startup")
            return self.log.exists() and f"web: listening on {self.port}" in (
                self.log.read_bytes()[old_size:].decode(errors="replace")
            )

        wait_for(ready, "editor did not start its web server", timeout=180)

    def drain(self):
        while self.pid is not None and (data := read_chunk(self.master, timeout=0)):
            self.terminal_output = (self.terminal_output + data)[-20000:]

    def stop(self):
        if self.pid is not None:
            stop(self.pid, self.master)
            self.pid = None

    def wait_for_exit(self):
        def exited():
            self.drain()
            status = poll_exit(self.pid)
            if status is not None:
                os.close(self.master)
                self.pid = None
                assert status == 0
                return True
            return False

        wait_for(exited, "editor did not exit cleanly")


class Browser:
    def __init__(self, directory, editor):
        self.editor = editor
        self.directory = directory
        self.process = self.connection = None
        self.sequence = 0
        self.network_urls = set()
        self.errors = []

    def __enter__(self):
        profile = self.directory / "chrome"
        with (self.directory / "chrome.log").open("wb") as log:
            self.process = subprocess.Popen([
                CHROME, "--headless", "--disable-gpu", "--no-first-run",
                "--remote-debugging-port=0", f"--user-data-dir={profile}",
                "--window-size=1100,750", "about:blank",
            ], stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
        try:
            port_file = profile / "DevToolsActivePort"
            port = wait_for(lambda: port_file.read_text().splitlines()[0] if port_file.exists() else None,
                            "Chrome did not start")
            with urllib.request.urlopen(f"http://127.0.0.1:{port}/json/list", timeout=5) as response:
                tabs = json.load(response)
            tab = next(tab for tab in tabs if tab["type"] == "page")
            self.connection = websocket.create_connection(tab["webSocketDebuggerUrl"],
                                                          timeout=10, suppress_origin=True)
            self.call("Network.enable")
            self.call("Runtime.enable")
            self.call("Log.enable")
            navigation = self.call("Page.navigate", url=self.editor.url)
            assert "errorText" not in navigation, navigation
            self.see("*scratch*")
            return self
        except Exception:
            if self.connection:
                diagnostic = self.evaluate("""({
                    url: location.href, title: document.title, html: document.documentElement.outerHTML.length,
                    terminal: typeof terminal, paintedSize: typeof paintedSize === 'undefined' ? null : paintedSize,
                    socket: typeof socket === 'undefined' ? null : socket.readyState,
                    body: document.body?.innerText
                })""")
                print("Browser diagnostics:", diagnostic, self.network_urls, self.errors, flush=True)
            self.__exit__()
            raise

    def __exit__(self, *_):
        if self.connection:
            self.connection.close()
        if self.process:
            try:
                os.killpg(self.process.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                self.process.wait(5)
            except subprocess.TimeoutExpired:
                os.killpg(self.process.pid, signal.SIGKILL)
                self.process.wait()

    def call(self, method, **params):
        self.editor.drain()
        self.sequence += 1
        self.connection.send(json.dumps(dict(id=self.sequence, method=method, params=params)))
        while True:
            message = json.loads(self.connection.recv())
            if message.get("method") == "Network.requestWillBeSent":
                self.network_urls.add(message["params"]["request"]["url"])
            elif message.get("method") == "Runtime.exceptionThrown":
                self.errors.append(message["params"]["exceptionDetails"])
            elif message.get("method") == "Log.entryAdded":
                entry = message["params"]["entry"]
                if entry["level"] == "error":
                    self.errors.append(entry)
            if message.get("id") == self.sequence:
                if "error" in message:
                    raise AssertionError(message["error"])
                return message["result"]

    def evaluate(self, expression):
        result = self.call("Runtime.evaluate", expression=expression, returnByValue=True)
        if "exceptionDetails" in result:
            raise AssertionError(result["result"].get("description", result["exceptionDetails"]))
        return result["result"].get("value")

    def screen(self):
        return self.evaluate(r"""
            typeof terminal === 'undefined' ? '' :
            Array.from({length: terminal.rows}, (_, i) =>
                terminal.buffer.active.getLine(i)?.translateToString(true)).join('\n')
        """)

    def see(self, text, timeout=12):
        try:
            wait_for(lambda: text in self.screen(), f"screen did not show {text!r}", timeout)
        except AssertionError as error:
            raise AssertionError(f"{error}\n{self.screen()}") from error

    def key(self, key, modifier=0):
        special = {"Enter": 13, "Backspace": 8, "Tab": 9, "Escape": 27, "ArrowDown": 40, "ArrowRight": 39,
                   "Home": 36, "End": 35, "PageDown": 34, "-": 189, ".": 190, "/": 191,
                   ">": 190, ":": 186}
        number = special.get(key, ord(key.upper()) if len(key) == 1 else 0)
        params = dict(key=key, windowsVirtualKeyCode=number, modifiers=modifier)
        typed = {"text": key} if not modifier and len(key) == 1 else {}
        self.call("Input.dispatchKeyEvent", type="keyDown", **params, **typed)
        self.call("Input.dispatchKeyEvent", type="keyUp", **params)

    def ctrl(self, key):
        self.key(key, 2)

    def meta(self, key):
        self.key(key, 1)

    def type(self, text):
        for character in text:
            self.key(character)

    def command(self, name):
        self.meta("x")
        self.see("M-x ")
        self.type(name)
        self.key("Enter")

    def paste(self, text):
        self.evaluate(f"""(() => {{
            const data = new DataTransfer();
            data.setData('text/plain', {json.dumps(text)});
            terminal.textarea.dispatchEvent(new ClipboardEvent('paste', {{
                clipboardData: data, bubbles: true, cancelable: true
            }}));
        }})()""")

    def open_file(self, file):
        self.ctrl("x")
        self.ctrl("f")
        self.see("Find file: ")
        self.paste(str(file))
        self.key("Enter")

    def switch_buffer(self, name):
        self.ctrl("x")
        self.key("b")
        self.see("Buffer: ")
        self.type(name)
        self.key("Enter")

    def save(self):
        self.ctrl("x")
        self.ctrl("s")

    def click_cell(self, row, col):
        x, y = self.evaluate(f"""(() => {{
            const rect = document.querySelector('.xterm-screen').getBoundingClientRect();
            return [rect.left + ({col} + 0.5) * rect.width / terminal.cols,
                    rect.top + ({row} + 0.5) * rect.height / terminal.rows];
        }})()""")
        for event_type in ("mousePressed", "mouseReleased"):
            self.call("Input.dispatchMouseEvent", type=event_type, x=x, y=y,
                      button="left", clickCount=1)

    def resize(self, width, height):
        before = self.evaluate("paintedSize")
        self.call("Emulation.setDeviceMetricsOverride", width=width, height=height,
                  deviceScaleFactor=1, mobile=False)
        wait_for(lambda: self.evaluate(f"paintedSize !== {json.dumps(before)} && "
                                      "paintedSize === terminal.rows + 'x' + terminal.cols"),
                 "web viewport did not redraw after resize")


def run(directory, editor):
    project = directory / "project"
    project.mkdir()
    sample = project / "sample.txt"
    sample.write_text("alpha\nbeta needle\n")
    large = project / "large.txt"
    large.write_text("".join(f"large line {i:06d}\n" for i in range(1, 100001)))
    source = project / "src" / "demo" / "core.clj"
    source.parent.mkdir(parents=True)
    source.write_text('(ns demo.core)\n(defn greet [name] (str "Hello " name))\n(greet "web")\n')
    (project / "deps.edn").write_text('{:paths ["src"]}\n')
    (project / "tests.sh").write_text(
        """printf '%s\\n' '<testsuite><testcase name="web"><failure>sample.txt:2 browser assertion</failure></testcase></testsuite>'\n"""
    )
    init_file = editor.config / "init.clj"
    init_file.write_text('(swap! nesmacs.api/*state* assoc :test-command "sh tests.sh")\n')

    def git(*args):
        return subprocess.run(["git", "-C", str(project), *args], check=True,
                              capture_output=True, text=True).stdout.strip()

    git("init", "-q")
    git("config", "user.name", "Nesmacs Web Test")
    git("config", "user.email", "nesmacs@example.test")
    git("config", "commit.gpgsign", "false")
    git("add", ".")
    git("commit", "-qm", "fixture")

    editor.start()
    with Browser(directory, editor) as browser:
        assert not any(url.startswith(("http:", "https:")) and not url.startswith(editor.url + "/")
                       for url in browser.network_urls), browser.network_urls
        browser.meta("x")
        browser.type("save")
        browser.see("M-x save")
        for _ in range(3):
            browser.key("Escape")
        browser.see("Quit")
        browser.open_file(sample)
        browser.see("beta needle")
        browser.type("A")
        browser.paste("paste-one\r\npaste-two")
        browser.type("B")
        browser.see("paste-twoBalpha")
        browser.ctrl("/")
        browser.see("paste-twoalpha")
        browser.ctrl("/")
        browser.see("Aalpha")
        browser.click_cell(1, 5)
        browser.call("Input.insertText", text="ñ😀")
        browser.call("Input.imeSetComposition", text="にほん", selectionStart=3, selectionEnd=3)
        browser.call("Input.insertText", text="日本")
        browser.see("beta ñ😀日本needle")
        assert browser.evaluate(
            "terminal.buffer.active.getLine(1).getCell(6).getWidth() === 2 && "
            "terminal.buffer.active.getLine(1).getCell(8).getChars() === '日'"
        ), "emoji and CJK must occupy separate cells"
        browser.save()
        wait_for(lambda: sample.read_text() == "Aalpha\nbeta ñ😀日本needle\n",
                 "Ctrl+X Ctrl+S did not save the exact text")

        browser.ctrl("x")
        browser.key("2")
        wait_for(lambda: browser.screen().count("sample.txt") == 2, "Ctrl+X 2 did not split")
        browser.ctrl("x")
        browser.key("1")
        browser.resize(430, 320)
        browser.see("sample.txt")
        browser.see("beta ñ😀日本needle")
        browser.resize(1100, 750)

        browser.ctrl("x")
        browser.type("pg")
        browser.paste("needle")
        browser.key("Enter")
        browser.see("./sample.txt:2:")
        browser.key("Enter")
        browser.see("sample.txt   L2")

        browser.ctrl("x")
        browser.key("g")
        browser.see("*Git Status*")
        browser.ctrl("n")
        browser.key("d")
        browser.see("+Aalpha")
        browser.switch_buffer("*Git Status*")
        browser.key("s")
        wait_for(lambda: git("diff", "--cached", "--name-only") == "sample.txt",
                 "Git status did not stage sample.txt")
        browser.key("c")
        browser.see("*Git Commit*")
        browser.paste("Browser smoke commit")
        browser.ctrl("c")
        browser.ctrl("c")
        wait_for(lambda: git("log", "-1", "--format=%s") == "Browser smoke commit",
                 "Git commit did not run from the web")

        browser.switch_buffer("sample.txt")
        browser.command("run-tests")
        browser.see("Test command")
        browser.key("Enter")
        browser.see("1 failed / 1 tests")
        browser.ctrl("n")
        browser.key("Enter")
        browser.see("sample.txt   L2")
        init_file.write_text(init_file.read_text() +
                             '(nesmacs.api/bind! "C-c j" #(assoc % :msg "WEB_CONFIG_RELOADED"))\n')
        browser.command("reload-config")
        browser.ctrl("c")
        browser.key("j")
        browser.see("WEB_CONFIG_RELOADED")

        browser.switch_buffer("*scratch*")
        browser.meta(">")
        browser.paste("web-browser-symbol\nweb-b")
        browser.ctrl("c")
        browser.key("i")
        browser.see("Complete: ")
        browser.type("web-b")
        browser.see("{web-b web-browser-symbol}")
        browser.ctrl("n")
        browser.ctrl("n")
        browser.see("Complete: web-browser-symbol")
        browser.key("Enter")
        wait_for(lambda: [line.strip() for line in browser.screen().splitlines()]
                 .count("web-browser-symbol") == 2,
                 "symbol completion did not update the buffer")
        print("PASS open, edit, IME, paste/undo, click, save, resize, search, Git, tests, reload, completion",
              flush=True)

        browser.open_file(large)
        browser.see("large line 000001")
        browser.meta(">")
        browser.see("large line 100000")
        browser.key("ArrowDown")
        browser.paste("RECOVERED_WEB")
        browser.see("RECOVERED_WEB")
        editor.stop()
        browser.see("RECOVERED_WEB")
        wait_for(lambda: "Reconnecting" in browser.evaluate("status.textContent"),
                 "browser did not report disconnect")
        editor.start()
        wait_for(lambda: browser.evaluate("paintedSize && !status.textContent"),
                 "browser did not reconnect", timeout=25)
        browser.see("RECOVERED_WEB")
        browser.save()
        wait_for(lambda: large.read_text().endswith("\nRECOVERED_WEB"),
                 "the recovered edit was not saved")
        print("PASS edit/save 100k lines and recover the acknowledged edit after SIGKILL", flush=True)

        lsp = os.environ.get("NESMACS_CLOJURE_LSP", "clojure-lsp")
        if os.environ.get("NESMACS_CLOJURE_LSP"):
            init_file.write_text(init_file.read_text() +
                                 f'(swap! nesmacs.api/*state* assoc :lsp-command [{json.dumps(lsp)}])\n')
            browser.command("reload-config")
        assert shutil.which(lsp), "Install clojure-lsp or set NESMACS_CLOJURE_LSP."
        browser.open_file(source)
        browser.see('(greet "web")')
        browser.click_cell(2, 2)
        browser.ctrl("c")
        browser.key("h")
        browser.see("LSP: ", timeout=70)
        browser.see("greet")
        browser.ctrl("c")
        browser.key("r")
        browser.see("*References*", timeout=20)
        browser.switch_buffer("core.clj")
        browser.click_cell(2, 2)
        browser.meta(".")
        browser.see("core.clj   L2")
        print("PASS hover, references and definition with clojure-lsp in the browser", flush=True)

        browser.ctrl("x")
        browser.ctrl("c")
        browser.see("Discard changes to *scratch*?")
        browser.key("y")
        browser.key("Enter")
        editor.wait_for_exit()


if __name__ == "__main__":
    if not CHROME:
        raise SystemExit("Install Chrome or Chromium, or set NESMACS_CHROME.")
    with tempfile.TemporaryDirectory(prefix="nesmacs-web-") as temporary:
        root = Path(temporary)
        editor = Editor(root)
        try:
            run(root, editor)
        except Exception:
            print("Terminal output:", editor.terminal_output[-2000:].decode(errors="replace"),
                  flush=True)
            if editor.log.exists():
                log = editor.log.read_text(errors="replace")
                print("Editor log:\n" + log[-5000:], flush=True)
            raise
        finally:
            editor.stop()
