"""Start an isolated editor and read its PTY until data, timeout or EOF."""
import errno
import fcntl
import os
import pty
import select
import signal
import struct
import termios
import time
from pathlib import Path


def start(home, *files, cols=80, env=None, jar=None):
    pid, fd = pty.fork()
    if pid == 0:
        os.chdir(Path(__file__).resolve().parents[2])
        os.environ.update(HOME=str(home), NESMACS_NREPL_PORT="0")
        os.environ.update(env or {})
        fcntl.ioctl(0, termios.TIOCSWINSZ, struct.pack("HHHH", 24, cols, 0, 0))
        if jar:
            os.execvp("java", ["java", "-Duser.home=" + str(home), "-jar", jar,
                               *map(str, files)])
        os.execvp("clojure", ["clojure", "-J-Duser.home=" + str(home), "-M:run",
                              *map(str, files)])
    return pid, fd


def read_chunk(fd, timeout=0.2):
    if not select.select([fd], [], [], timeout)[0]:
        return None
    try:
        return os.read(fd, 65536)
    except OSError as error:
        if error.errno == errno.EIO:
            return b""
        raise


def read_until(fd, matches, deadline, out=b""):
    while not matches(out) and time.monotonic() < deadline:
        chunk = read_chunk(fd)
        if chunk == b"":
            break
        if chunk:
            out += chunk
    return out, matches(out)


def poll_exit(pid):
    done, status = os.waitpid(pid, os.WNOHANG)
    if done:
        return os.waitstatus_to_exitcode(status)


def stop(pid, fd):
    try:
        try:
            if poll_exit(pid) is None:
                try:
                    os.killpg(pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                os.waitpid(pid, 0)
        except ChildProcessError:
            pass
    finally:
        os.close(fd)
