#!/usr/bin/env python3

import argparse
import json
import pathlib
import subprocess
import sys
import tempfile


# Run one BAli-Phy command in the isolated work directory and retain both output streams.
def run_command(command, work_directory):
    return subprocess.run(
        command,
        cwd=work_directory,
        text=True,
        capture_output=True,
    )


# Verify runtime-mode behavior that requires reusing source retained by an earlier infer process.
# This becomes obsolete if infer no longer retains runnable Haskell source.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--wrapper", action="append", default=[])
    parser.add_argument("executable")
    parser.add_argument("package_path")
    args = parser.parse_args()

    with tempfile.TemporaryDirectory(prefix="bali-phy-generated-program-") as tmp:
        work_directory = pathlib.Path(tmp)
        (work_directory / "input.fasta").write_text(
            ">one\nACGT\n>two\nACGT\n",
            encoding="utf-8",
        )

        generate = args.wrapper + [
            args.executable,
            "--seed=1",
            args.package_path,
            "infer",
            "input.fasta",
            "--imodel=none",
            "--smodel=TN93",
            "--iterations=0",
            "--name=generated",
        ]
        generated = run_command(generate, work_directory)
        if generated.returncode != 0:
            raise AssertionError(generated.stdout + generated.stderr)

        source = work_directory / "generated-1" / "BAliPhy.Main.hs"
        if not source.is_file():
            raise AssertionError("the initial run did not retain BAliPhy.Main.hs")

        run_generated = args.wrapper + [
            args.executable,
            "--seed=1",
            args.package_path,
            "run",
            str(source.relative_to(work_directory)),
        ]

        directories_before_test = {path for path in work_directory.rglob("*") if path.is_dir()}
        logger_files_before_test = {
            path for path in work_directory.rglob("C1.*") if path.is_file()
        }
        test_result = run_command(run_generated + ["--test"], work_directory)
        if test_result.returncode != 0:
            raise AssertionError(test_result.stdout + test_result.stderr)
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
        first = run_command(standalone, work_directory)
        if first.returncode != 0:
            raise AssertionError(first.stdout + first.stderr)
        if "Beginning MCMC computations." not in first.stdout:
            raise AssertionError("the standalone program did not report the start of execution")

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

        fixed_generate = args.wrapper + [
            args.executable,
            "--seed=1",
            args.package_path,
            "infer",
            "input.fasta",
            "--fix=alignment",
            "--test",
        ]
        fixed_generated = run_command(fixed_generate, work_directory)
        if fixed_generated.returncode != 0:
            raise AssertionError(fixed_generated.stdout + fixed_generated.stderr)

        fixed_source = work_directory / "BAliPhy.Main.hs"
        fixed_run = args.wrapper + [
            args.executable,
            "--seed=1",
            args.package_path,
            "run",
            str(fixed_source.relative_to(work_directory)),
            "--name=fixed-retained",
        ]
        fixed_result = run_command(fixed_run, work_directory)
        fixed_error = "Currently --fix=alignment only works with --test."
        if fixed_result.returncode == 0 or fixed_error not in fixed_result.stderr:
            raise AssertionError(fixed_result.stdout + fixed_result.stderr)
        if (work_directory / "fixed-retained-1").exists():
            raise AssertionError("the retained fixed-alignment program created an output directory")

    return 0


if __name__ == "__main__":
    sys.exit(main())
