#!/usr/bin/env python3
"""kill -9 recovery: edit a file, SIGKILL the editor before the 5s snapshot,
restart with the same HOME, and require the edit to be back on screen.

Run with `make crash`. Uses a temp HOME so it never touches the real ~/.nesmacs.
"""
import os
import shutil
import sys
import tempfile
import time
from pathlib import Path

from editor_pty import read_until, start, stop

TIMEOUT = 180
MARKER = b"KILL9MARKER"

home = Path(tempfile.mkdtemp(prefix="nesmacs-crash-"))
(home / ".nesmacs").mkdir()
doc = home / "doc.txt"
doc.write_text("needle-line\n")


ok = False
pid = None
try:
    # 1. open the file, type a marker at point 0, and die before the snapshot.
    pid, fd = start(home)
    out, _ = read_until(fd, lambda data: b"*scratch*" in data, time.monotonic() + TIMEOUT)
    os.write(fd, b"\x18\x06")                      # C-x C-f
    os.write(fd, str(doc).encode() + b"\r")
    out, opened = read_until(fd, lambda data: b"needle-line" in data, time.monotonic() + TIMEOUT, out)
    if not opened:
        raise RuntimeError("file never opened")
    os.write(fd, MARKER)
    # Keep draining the pty: rendering a full screen per keystroke would fill
    # the pty buffer and block the editor (looks like lost input) if we slept.
    out, typed = read_until(fd, lambda data: MARKER in data, time.monotonic() + TIMEOUT, out)
    if not typed:
        raise RuntimeError("marker never rendered")
    stop(pid, fd)                                  # before the next snapshot
    pid = None

    journal = home / ".nesmacs" / "journal.edn"
    had_journal = journal.exists()

    # 2. restart: the journal must rebuild the unsaved edit.
    pid, fd = start(home)
    out, restored = read_until(fd, lambda data: MARKER in data, time.monotonic() + TIMEOUT)
    os.write(fd, b"\x18\x03")                      # C-x C-c
    read_until(fd, lambda _: False, time.monotonic() + 3, out)      # drain the exit

    ok = had_journal and restored
    print("journal=%s restored=%s" % (had_journal, restored))
except Exception as e:  # noqa: BLE001 - report and fail
    print("FAIL: %s" % e)
finally:
    if pid is not None:
        stop(pid, fd)
    shutil.rmtree(home)

print("PASS" if ok else "FAIL")
sys.exit(0 if ok else 1)