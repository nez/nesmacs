#!/usr/bin/env python3
"""Terminal smoke test: start nesmacs in a pty, load config, run a job, exit.

Run with `make pty`. Uses a temp HOME (and -Duser.home) so it never touches the
real ~/.nesmacs. Asserts the startup log (config loaded, clean exit) and that a
M-! job streams into its buffer while the editor keeps accepting keys.
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

home = Path(tempfile.mkdtemp(prefix="nesmacs-pty-"))
(home / ".nesmacs").mkdir()
(home / ".nesmacs" / "init.clj").write_text(
    "(ns smoke.init (:require [nesmacs.api :as api]))\n"
    "(defn hello \"Say hello.\" [s] (assoc s :msg \"hello\"))\n"
    "(api/bind! \"C-c h\" #'hello)\n"
)

pid, fd = pty.fork()
if pid == 0:
    os.chdir(ROOT)
    os.environ["HOME"] = str(home)
    os.environ["NESMACS_NREPL_PORT"] = "0"
    os.execvp("clojure", ["clojure", "-J-Duser.home=" + str(home), "-M:run"])

fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 24, 80, 0, 0))

out = b""
start = time.time()
sent_job = False
job_seen_at = None
sent_exit = False
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
    if not sent_job and b"*scratch*" in out:
        # M-! yes | head -c 300000 RET: a firehose into a job buffer.
        os.write(fd, b"\x1b!yes | head -c 300000\r")
        sent_job = True
    elif sent_job and job_seen_at is None and b"*Shell Command*" in out:
        job_seen_at = time.time()
    elif job_seen_at and not sent_exit and time.time() - job_seen_at > 0.2:
        # Output is still streaming; the editor must still take C-x C-c.
        os.write(fd, b"\x18\x03")
        sent_exit = True

try:
    os.kill(pid, 0)
    alive = True
except ProcessLookupError:
    alive = False
if alive:
    os.kill(pid, signal.SIGKILL)
    os.waitpid(pid, 0)

log = ""
logfile = home / ".nesmacs" / "debug.log"
if logfile.exists():
    log = logfile.read_text(errors="replace")

ok = (sent_job and job_seen_at is not None and sent_exit and exited
      and "loaded init.clj" in log and "nesmacs exiting" in log)
print("rendered=%s job_buffer=%s exited=%s bytes=%d"
      % (b"*scratch*" in out, job_seen_at is not None, exited, len(out)))
print("config_loaded=%s clean_exit=%s"
      % ("loaded init.clj" in log, "nesmacs exiting" in log))

for child in home.rglob("*"):
    if child.is_file():
        child.unlink()
for child in sorted(home.rglob("*"), reverse=True):
    if child.is_dir():
        child.rmdir()
home.rmdir()

print("PASS" if ok else "FAIL: log:\n" + log[-2000:])
sys.exit(0 if ok else 1)