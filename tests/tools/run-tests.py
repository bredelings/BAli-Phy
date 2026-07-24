#!/usr/bin/env python3

import argparse
import difflib
import os
from pathlib import Path
import shlex
import shutil
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


# Run one directory-defined test, retaining its captured results until the checks finish.
def run_test(directory, command):
    arguments = shlex.split(read_file(directory, "args", ""), comments=True)
    environment = os.environ.copy()
    environment["COLUMNS"] = "110"
    results = directory / "results"
    if results.exists():
        shutil.rmtree(results)
    results.mkdir()

    result = subprocess.run(
        command + arguments,
        cwd=directory,
        input=read_file(directory, "input", ""),
        capture_output=True,
        check=False,
        env=environment,
        text=True,
    )

    (results / "output").write_text(result.stdout, encoding="utf-8")
    (results / "error").write_text(result.stderr, encoding="utf-8")
    (results / "exit").write_text(f"{result.returncode}\n", encoding="utf-8")

    failures = []
    expected_error = read_file(directory, "error", "")
    expected_exit = int(read_file(directory, "exit", "0").strip())
    if (directory / "output").exists():
        expected_output = read_file(directory, "output", "")
        if result.stdout != expected_output:
            failures.append(text_diff("output", expected_output, result.stdout))
    if result.stderr != expected_error:
        failures.append(text_diff("error", expected_error, result.stderr))
    if result.returncode != expected_exit:
        failures.append(f"expected exit {expected_exit}, obtained {result.returncode}\n")
    return failures, results


# Print directory-defined test names relative to the requested root.
def list_tests(root):
    tests = sorted(path.parent for path in root.rglob("args"))
    if not tests:
        print(f"No tests found below {root}", file=sys.stderr)
        return 1
    for test in tests:
        print(test.relative_to(root))
    return 0


# Run one named test and preserve its results only when a check fails.
def run_named_test(directory, command):
    if not (directory / "args").exists():
        print(f"No test found in {directory}", file=sys.stderr)
        return 1

    failures, results = run_test(directory, command)
    if failures:
        for failure in failures:
            print(failure, end="" if failure.endswith("\n") else "\n")
        print(f"Test results retained in {results}", file=sys.stderr)
        return 1

    shutil.rmtree(results)
    return 0


# Parse the list/run command line used by Meson and direct test invocations.
def main():
    parser = argparse.ArgumentParser(description="Run directory-defined command-line tool tests.")
    commands = parser.add_subparsers(dest="command", required=True)

    list_parser = commands.add_parser("list")
    list_parser.add_argument("root", type=Path)

    run_parser = commands.add_parser("run")
    run_parser.add_argument("--wrapper", action="append", default=[])
    run_parser.add_argument("directory", type=Path)
    run_parser.add_argument("binary", type=Path)
    arguments = parser.parse_args()

    if arguments.command == "list":
        return list_tests(arguments.root.resolve())

    directory = arguments.directory.resolve()
    command = arguments.wrapper + [str(arguments.binary.resolve())]
    return run_named_test(directory, command)


if __name__ == "__main__":
    sys.exit(main())
