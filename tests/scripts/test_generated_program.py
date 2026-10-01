#!/usr/bin/env python3

import argparse
import json
import pathlib
import shlex
import subprocess
import sys
import tempfile
import time


# Identify and time each phase, checking its exit status while retaining output for diagnostics.
def run_command(phase, command, work_directory, expected_exit=0):
    print(f"Starting: {phase}", flush=True)
    start = time.monotonic()
    result = subprocess.run(command, cwd=work_directory, encoding="utf-8", capture_output=True)
    print(f"Finished: {phase} ({time.monotonic() - start:.2f}s)", flush=True)
    if result.returncode != expected_exit:
        raise AssertionError(f"{phase}: expected exit {expected_exit}, got {result.returncode}\n"
                             f"Command: {shlex.join(command)}\n{result.stdout}{result.stderr}")
    return result


# Reuse generated source in both runtime modes; single-command tests cannot check this handoff.
# This becomes obsolete if infer no longer retains runnable source.
def standalone_test(command, work_directory):
    generate = command + [
        "input.fasta",
        "--imodel=none",
        "--smodel=TN93",
        "--iterations=0",
        "--name=generated",
    ]
    run_command("generate standalone source", generate, work_directory)

    source = work_directory / "generated-1" / "BAliPhy.Main.hs"
    if not source.is_file():
        raise AssertionError("the initial run did not retain BAliPhy.Main.hs")

    run_generated = command + [
        "run",
        str(source.relative_to(work_directory)),
    ]

    directories_before_test = {path for path in work_directory.rglob("*") if path.is_dir()}
    logger_files_before_test = {
        path for path in work_directory.rglob("C1.*") if path.is_file()
    }
    run_command("execute test mode", run_generated + ["--test"], work_directory)
    directories_after_test = {path for path in work_directory.rglob("*") if path.is_dir()}
    logger_files_after_test = {
        path for path in work_directory.rglob("C1.*") if path.is_file()
    }
    if directories_after_test != directories_before_test:
        raise AssertionError("the retained program created a directory in test mode")
    if logger_files_after_test != logger_files_before_test:
        raise AssertionError("the retained program created a logger file in test mode")

    output_directory = pathlib.Path("standalone") / "nested"
    (work_directory / output_directory).mkdir(parents=True)
    standalone = run_generated + [
        "--output-dir",
        str(output_directory),
        "--log-format=json",
    ]
    run_command("execute normal mode", standalone, work_directory)

    output_paths = [
        work_directory / output_directory / name
        for name in ["C1.log.json", "C1.trees"]
    ]
    if not all(path.is_file() for path in output_paths):
        raise AssertionError(f"the standalone program did not create its log files: {output_paths}")

    # Preserve logging of structured model parameters across representation changes;
    # successful generated-program execution alone does not establish that the field survived.
    log_records = output_paths[0].read_text(encoding="utf-8").splitlines()
    sample = json.loads(log_records[1])
    frequencies = sample["parameters//"]["S1/"]["TN93:pi"]
    if not isinstance(frequencies, dict) or set(frequencies) != {"A", "C", "G", "T"}:
        raise AssertionError(f"TN93 frequencies were not logged as a JSON object: {frequencies}")

    if (work_directory / output_directory / "C1.log").exists():
        raise AssertionError("the standalone program ignored its runtime log format")
    if (work_directory / output_directory / "C1.run.json").exists():
        raise AssertionError("the standalone program unexpectedly created the C++-owned run manifest")


# Bypass early C++ validation to check the guard in retained Haskell before it creates output.
# This becomes obsolete if fixed alignments become valid during MCMC.
def fixed_alignment_test(command, work_directory):
    fixed_generate = command + [
        "input.fasta",
        "--fix=alignment",
        "--test",
    ]
    run_command("generate fixed-alignment source", fixed_generate, work_directory)

    fixed_source = work_directory / "BAliPhy.Main.hs"
    fixed_run = command + [
        "run",
        str(fixed_source.relative_to(work_directory)),
        "--name=fixed-retained",
    ]
    fixed_result = run_command("reject fixed-alignment execution", fixed_run, work_directory, expected_exit=1)
    fixed_error = "Currently --fix=alignment only works with --test."
    if fixed_error not in fixed_result.stderr:
        raise AssertionError("Missing fixed-alignment diagnostic:\n" + fixed_result.stdout + fixed_result.stderr)
    if (work_directory / "fixed-retained-1").exists():
        raise AssertionError("the retained fixed-alignment program created an output directory")


# Each Meson case owns its work directory; only commands that reuse generated source run together.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--case", required=True, choices=["standalone", "fixed-alignment"])
    parser.add_argument("--wrapper", action="append", default=[])
    parser.add_argument("executable")
    parser.add_argument("package_path")
    args = parser.parse_args()

    with tempfile.TemporaryDirectory(prefix=f"bali-phy-generated-{args.case}-") as tmp:
        work_directory = pathlib.Path(tmp)
        (work_directory / "input.fasta").write_text(">one\nACGT\n>two\nACGT\n", encoding="utf-8")
        command = args.wrapper + [args.executable, "--seed=1", args.package_path]
        if args.case == "standalone":
            standalone_test(command, work_directory)
        else:
            fixed_alignment_test(command, work_directory)
    return 0


if __name__ == "__main__":
    sys.exit(main())
