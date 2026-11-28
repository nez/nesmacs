#!/usr/bin/env python3
"""Project workflow: blame, hunk staging, commits, test results and forge.

Drives the real editor in a pty against a temp git project. `gh` is a stub that
prints the same JSON the real CLI emits: the test has no GitHub credentials,
but it still exercises nesmacs.forge's parse and list buffer. Run with `make pty`.
Uses a temp HOME (and -Duser.home) so it never touches the real ~/.nesmacs.
"""
import os
import re
import shutil
import signal
import stat
import subprocess
import sys
import tempfile
import time
from pathlib import Path

from editor_pty import read_chunk, read_until as wait_for_output, start

TIMEOUT = 60
ANSI = re.compile(rb"\x1b\[[0-9;?]*[ -/]*[@-~]|\x1b\][^\x07]*(?:\x07|\x1b\\)|\x1b[()][A-Z0-9]|\x1b[=>]")

home = Path(tempfile.mkdtemp(prefix="nesmacs-lists-"))
proj = home / "proj"
proj.mkdir()
doc = proj / "doc.txt"
doc.write_text("alpha line\nbeta line\n")


def git(*args):
    return subprocess.run(["git", "-C", str(proj), "-c", "user.name=Test User",
                           "-c", "user.email=test@example.com", *args],
                          check=True, capture_output=True)


git("init", "-q")
git("config", "user.name", "Test User")
git("config", "user.email", "test@example.com")
git("config", "commit.gpgsign", "false")
git("add", "doc.txt")
git("commit", "-qm", "first")
git("remote", "add", "origin", "https://github.com/example/demo.git")
# An unstaged hunk for `git-stage-hunk`.
doc.write_text("alpha line\nBETA changed\n")

# A JUnit report with one failure at t.js:2; line 2 carries the jump marker.
(proj / "t.js").write_text(
    '// pty list fixture\n'
    'const marker = "BOOMLINE";\n'
    "console.log('<testsuite tests=\"2\">');\n"
    "console.log('<testcase name=\"ok\"/>');\n"
    "console.log('<testcase name=\"boom\"><failure message=\"t.js:2 boom\"/></testcase>');\n"
    "console.log('</testsuite>');\n"
)
(proj / "package.json").write_text(
    '{"name":"demo","version":"1.0.0","private":true,'
    '"scripts":{"test":"node t.js"}}\n'
)

bin_dir = home / "bin"
bin_dir.mkdir()
gh = bin_dir / "gh"
gh.write_text(
    "#!/bin/sh\n"
    "cat <<'JSON'\n"
    '[{"number":7,"title":"Fix the thing","state":"OPEN",'
    '"headRefName":"fix-branch","url":"https://github.com/example/demo/pull/7"}]\n'
    "JSON\n"
)
gh.chmod(gh.stat().st_mode | stat.S_IXUSR)

pid, fd = start(home, doc, cols=100, env={"PATH": str(bin_dir) + ":" + os.environ["PATH"]})

out = b""
checks = []


def plain(b):
    return ANSI.sub(b"", b)


def read_until(needle, seconds=TIMEOUT, label=None):
    global out
    target = needle.encode() if isinstance(needle, str) else needle
    out, found = wait_for_output(fd, lambda data: target in plain(data),
                                 time.monotonic() + seconds, out)
    checks.append((label or needle, found))
    return found


def wait_exit(seconds=30):
    global out
    end = time.time() + seconds
    while time.time() < end:
        data = read_chunk(fd)
        if data == b"":
            return True
        if data:
            out += data
        done, _ = os.waitpid(pid, os.WNOHANG)
        if done:
            return True
    return False


def keys(b):
    os.write(fd, b)


ok = True
try:
    ok &= read_until("alpha line", label="source opened from the launch argument")

    keys(b"\x1bxgit-blame\r")                              # M-x git-blame
    ok &= read_until("Blame ", label="blame list opened")
    ok &= read_until("Test User", label="blame shows the author")

    keys(b"\x18bdoc.txt\r")                                # C-x b back to source
    keys(b"\x1bxgit-stage-hunk\r")                         # M-x git-stage-hunk
    ok &= read_until("Unstaged hunks", label="hunks list opened")
    keys(b"\x0es")                                         # C-n past the header, stage
    ok &= read_until("Staged hunk", label="hunk staged")
    cached = subprocess.run(["git", "-C", str(proj), "diff", "--cached",
                             "--name-only"], capture_output=True, text=True).stdout
    checks.append(("index contains doc.txt", "doc.txt" in cached))
    ok &= "doc.txt" in cached

    keys(b"\x18bdoc.txt\r\x18g")                           # C-x b doc.txt, then C-x g
    ok &= read_until("s stage  u unstage  c commit", label="git status opens from a file")
    keys(b"c")
    ok &= read_until("*Git Commit*", label="commit buffer opened")
    keys(b"PTY workflow commit\x03\x03")                   # C-c C-c
    ok &= read_until("] PTY workflow commit", label="commit succeeded in the editor")
    committed = git("log", "-1", "--format=%s").stdout.strip() == b"PTY workflow commit"
    checks.append(("commit recorded by Git", committed))
    ok &= committed

    keys(b"cSecond commit\x03\x03")                        # The index is empty now.
    ok &= read_until("Error: On branch", label="failed commit keeps the editor open")
    keys(b"\x03\x0b")                                      # C-c C-k
    ok &= read_until("Commit aborted", label="failed commit can be aborted")

    keys(b"\x18bdoc.txt\r")
    keys(b"\x1bxrun-tests\r\r")                            # M-x run-tests, accept default
    ok &= read_until("1 failed / 2 tests", label="test report parsed")
    keys(b"\x0e\r")                                        # C-n RET on the failure
    ok &= read_until("BOOMLINE", label="failure jumps to the source line")

    keys(b"\x1bxforge-prs\r")                              # M-x forge-prs (stub gh)
    ok &= read_until("#7", label="forge list opened")
    ok &= read_until("Fix the thing", label="forge entry parsed")

    keys(b"\x18\x03")                                      # C-x C-c
    exited = wait_exit()
    checks.append(("clean exit", exited))
    ok &= exited
finally:
    try:
        os.kill(pid, signal.SIGKILL)
    except ProcessLookupError:
        pass
    try:
        os.waitpid(pid, 0)
    except ChildProcessError:
        pass

log = ""
logfile = home / ".nesmacs" / "debug.log"
if logfile.exists():
    log = logfile.read_text(errors="replace")
ok &= "nesmacs exiting" in log
checks.append(("debug.log clean exit", "nesmacs exiting" in log))
ok &= "Exception" not in log and "Uncaught" not in log

for name, passed in checks:
    print("%-40s %s" % (name, "ok" if passed else "FAIL"))

shutil.rmtree(home, ignore_errors=True)
print("PASS" if ok else "FAIL")
sys.exit(0 if ok else 1)
