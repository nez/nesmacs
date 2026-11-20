#!/usr/bin/env python3
"""Terminal smoke test: start xmas in a pty, load config, exit with C-x C-c.

Run with `make pty`. Uses a temp HOME (and -Duser.home) so it never touches the
real ~/.xmas. Asserts the startup log: config loaded and a clean exit.
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
TIMEOUT = 90

home = Path(tempfile.mkdtemp(prefix="xmas-pty-"))
(home / ".xmas").mkdir()
(home / ".xmas" / "init.clj").write_text(
    "(ns smoke.init (:require [xmas.api :as api]))\n"
    "(defn hello \"Say hello.\" [s] (assoc s :msg \"hello\"))\n"
    "(api/bind! \"C-c h\" #'hello)\n"
)

pid, fd = pty.fork()
if pid == 0:
    os.chdir(ROOT)
    os.environ["HOME"] = str(home)
    os.execvp("clojure", ["clojure", "-J-Duser.home=" + str(home), "-M:run"])

fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))

out = b""
start = time.time()
sent = False
exited = False
while time.time() - start < TIMEOUT:
    r, _, _ = select.select([fd], [], [], 0.2)
    if r:
        try:
            data = os.read(fd, 65536)
        except OSError:
            exited = True
            break
        if not data:
            exited = True
            break
        out += data
    if not sent and b"*scratch*" in out:
        os.write(fd, b"\x18\x03")  # C-x C-c
        sent = True

try:
    os.kill(pid, 0)
    alive = True
except ProcessLookupError:
    alive = False
if alive:
    os.kill(pid, signal.SIGKILL)
    os.waitpid(pid, 0)

log = ""
logfile = home / ".xmas" / "debug.log"
if logfile.exists():
    log = logfile.read_text(errors="replace")

ok = sent and "loaded init.clj" in log and "xmas exiting" in log
print("rendered=%s sent_keys=%s exited=%s bytes=%d" % (b"*scratch*" in out, sent, exited, len(out)))
print("config_loaded=%s clean_exit=%s" % ("loaded init.clj" in log, "xmas exiting" in log))

for child in home.rglob("*"):
    if child.is_file():
        child.unlink()
for child in sorted(home.rglob("*"), reverse=True):
    if child.is_dir():
        child.rmdir()
home.rmdir()

print("PASS" if ok else "FAIL: log:\n" + log[-2000:])
sys.exit(0 if ok else 1)