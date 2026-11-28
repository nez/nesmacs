"""Start an isolated editor and read its PTY until data, timeout or EOF."""
import errno
import fcntl
import os
import pty
import select
import struct
import termios
import time
from pathlib import Path


def start(home, *files, cols=80, env=None):
    pid, fd = pty.fork()
    if pid == 0:
        os.chdir(Path(__file__).resolve().parents[2])
        os.environ.update(HOME=str(home), NESMACS_NREPL_PORT="0")
        os.environ.update(env or {})
        os.execvp("clojure", ["clojure", "-J-Duser.home=" + str(home), "-M:run",
                              *map(str, files)])
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", 24, cols, 0, 0))
    return pid, fd


def read_chunk(fd):
    if not select.select([fd], [], [], 0.2)[0]:
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
