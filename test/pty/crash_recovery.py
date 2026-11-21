#!/usr/bin/env python3
"""kill -9 recovery: edit a file, SIGKILL the editor before the 5s snapshot,
restart with the same HOME, and require the edit to be back on screen.

Run with `make crash`. Uses a temp HOME so it never touches the real ~/.xmas.
"""
import fcntl
import os
import pty
import select
import signal
import struct
import sys
import tempfile
import termios
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
TIMEOUT = 180
MARKER = b"KILL9MARKER"

home = Path(tempfile.mkdtemp(prefix="xmas-crash-"))
(home / ".xmas").mkdir()
doc = home / "doc.txt"
doc.write_text("needle-line\n")


def start():
    pid, fd = pty.fork()
    if pid == 0:
        os.chdir(ROOT)
        os.environ["HOME"] = str(home)
        os.execvp("clojure", ["clojure", "-J-Duser.home=" + str(home), "-M:run"])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))
    return pid, fd


def read_until(fd, needle, deadline, out=b""):
    while time.time() < deadline:
        r, _, _ = select.select([fd], [], [], 0.2)
        if r:
            try:
                data = os.read(fd, 65536)
            except OSError:
                break
            if not data:
                break
            out += data
            if needle in out:
                return out, True
    return out, needle in out


def kill9(pid):
    try:
        os.kill(pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    try:
        os.waitpid(pid, 0)
    except ChildProcessError:
        pass


ok = False
try:
    # 1. open the file, type a marker at point 0, and die before the snapshot.
    pid, fd = start()
    out, _ = read_until(fd, b"*scratch*", time.time() + TIMEOUT)
    os.write(fd, b"\x18\x06")                      # C-x C-f
    os.write(fd, str(doc).encode() + b"\r")
    out, opened = read_until(fd, b"needle-line", time.time() + TIMEOUT, out)
    if not opened:
        raise RuntimeError("file never opened")
    os.write(fd, MARKER)
    # Keep draining the pty: rendering a full screen per keystroke would fill
    # the pty buffer and block the editor (looks like lost input) if we slept.
    out, typed = read_until(fd, MARKER, time.time() + TIMEOUT, out)
    if not typed:
        raise RuntimeError("marker never rendered")
    kill9(pid)                                     # before the next snapshot

    journal = home / ".xmas" / "journal.edn"
    had_journal = journal.exists()

    # 2. restart: the journal must rebuild the unsaved edit.
    pid, fd = start()
    out, restored = read_until(fd, MARKER, time.time() + TIMEOUT)
    os.write(fd, b"\x18\x03")                      # C-x C-c
    read_until(fd, b"", time.time() + 3, out)      # drain the exit
    kill9(pid)

    ok = had_journal and restored
    print("journal=%s restored=%s" % (had_journal, restored))
except Exception as e:  # noqa: BLE001 - report and fail
    print("FAIL: %s" % e)
finally:
    for child in home.rglob("*"):
        if child.is_file():
            child.unlink()
    for child in sorted(home.rglob("*"), reverse=True):
        if child.is_dir():
            child.rmdir()
    home.rmdir()

print("PASS" if ok else "FAIL")
sys.exit(0 if ok else 1)