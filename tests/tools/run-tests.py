#!/usr/bin/env python3

import argparse
import difflib
import os
from pathlib import Path
import shlex
import subprocess
import sys


# Read a test file exactly, returning the supplied default when it is absent.
def read_file(directory, name, default):
    filename = directory / name
    if filename.exists():
        return filename.read_text(encoding="utf-8")
    return default


# Format an exact text mismatch as a unified diff.
def text_diff(name, expected, obtained):
    return "".join(
        difflib.unified_diff(
            expected.splitlines(keepends=True),
            obtained.splitlines(keepends=True),
            fromfile=f"expected-{name}",
            tofile=f"obtained-{name}",
        )
    )


# Run one directory-defined test and return its failure descriptions.
def run_test(directory, command):
    arguments = shlex.split(read_file(directory, "args", ""), comments=True)
    environment = os.environ.copy()
    environment["COLUMNS"] = "110"
    result = subprocess.run(
        command + arguments,
        cwd=directory,
        input=read_file(directory, "input", ""),
        capture_output=True,
        check=False,
        env=environment,
        text=True,
    )

    failures = []
    expected_output = read_file(directory, "output", "")
    expected_error = read_file(directory, "error", "")
    expected_exit = int(read_file(directory, "exit", "0").strip())
    if result.stdout != expected_output:
        failures.append(text_diff("output", expected_output, result.stdout))
    if result.stderr != expected_error:
        failures.append(text_diff("error", expected_error, result.stderr))
    if result.returncode != expected_exit:
        failures.append(f"expected exit {expected_exit}, obtained {result.returncode}\n")
    return failures


# Discover and run every declarative command test below the requested root.
def main():
    parser = argparse.ArgumentParser(description="Run directory-defined command-line tool tests.")
    parser.add_argument("--wrapper", action="append", default=[])
    parser.add_argument("root", type=Path)
    parser.add_argument("binary", type=Path)
    arguments = parser.parse_args()

    root = arguments.root.resolve()
    command = arguments.wrapper + [str(arguments.binary.resolve())]
    failed = 0
    tests = sorted(path.parent for path in root.rglob("args"))
    if not tests:
        print(f"No tests found below {root}", file=sys.stderr)
        return 1

    for test in tests:
        failures = run_test(test, command)
        name = test.relative_to(root)
        if failures:
            failed += 1
            print(f"{name}: FAIL")
            for failure in failures:
                print(failure, end="" if failure.endswith("\n") else "\n")
        else:
            print(f"{name}: ok")

    print(f"{len(tests) - failed} passed; {failed} failed")
    return failed != 0


if __name__ == "__main__":
    sys.exit(main())
