#!/usr/bin/env python3
import io
from io import UnsupportedOperation
import sys
import os

import img2orgtable_core
from img2orgtable_core import Error
from pathlib import Path

# silent stdout/err
def _silence_fds():
    try:
        stdout_fd = sys.stdout.fileno()
        stderr_fd = sys.stderr.fileno()
    except (io.UnsupportedOperation, AttributeError): 
        return None

    devnull_fd = os.open(os.devnull, os.O_WRONLY)

    saved_stdout = os.dup(stdout_fd)
    saved_stderr = os.dup(stderr_fd)

    os.dup2(devnull_fd, stdout_fd)
    os.dup2(devnull_fd, stderr_fd)

    return (saved_stdout, saved_stderr, devnull_fd, stdout_fd, stderr_fd)


def _restore_fds(token):
    if token is None: 
        return
    saved_stdout, saved_stderr, devnull_fd, stdout_fd, stderr_fd = token
    os.dup2(saved_stdout, stdout_fd)
    os.dup2(saved_stderr, stderr_fd)
    os.close(saved_stdout)
    os.close(saved_stderr)
    os.close(devnull_fd)


def main(argv=None):
    tok = _silence_fds()

    argv = argv or sys.argv[1:]
    err: Error = Error()
    
    if len(argv) < 1 or not Path(argv[0]).exists():
        err.code = 64
        err.mes = "usage: img2org.py IMAGE"
        print(err.mes, file=sys.stderr)
        sys.exit(err.code)

    table = img2orgtable_core.extract_table(argv[0], err)

    if err.code != 0: 
        print(err.mes, file=sys.stderr)
        sys.exit(err.code)

    if table is None:
        # visible again
        print("# no table detected", file=sys.stderr)
        sys.exit(2)

    _restore_fds(tok)
    print(img2orgtable_core.to_org(table))

if __name__ == "__main__":
    main()
