#!/usr/bin/env python3

import argparse
import pathlib
import shutil
import subprocess
import sys
import tempfile


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--wrapper', action='append', default=[])
    parser.add_argument('executable')
    parser.add_argument('test_args', nargs=argparse.REMAINDER)
    args = parser.parse_args()

    executable = pathlib.Path(args.executable)

    with tempfile.TemporaryDirectory(prefix='runtime-ast-serialization-') as tmpdir:
        host_executable = pathlib.Path(tmpdir) / 'bali-phy.exe'
        shutil.copy2(executable, host_executable)

        cmd = args.wrapper + [str(host_executable)] + args.test_args
        return subprocess.run(cmd).returncode


if __name__ == '__main__':
    sys.exit(main())
