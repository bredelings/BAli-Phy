#!/usr/bin/env python3

import argparse
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


# Exercise the public interface of a generated alignment program, including overwrite protection.
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
            "input.fasta",
            "--imodel=none",
            "--iter=0",
            "--name=generated",
            "--seed=1",
            args.package_path,
        ]
        generated = run_command(generate, work_directory)
        if generated.returncode != 0:
            raise AssertionError(generated.stdout + generated.stderr)

        source = work_directory / "generated-1" / "BAliPhy.Main.hs"
        if not source.is_file():
            raise AssertionError("the initial run did not retain BAliPhy.Main.hs")

        run_generated = args.wrapper + [
            args.executable,
            "run",
            str(source.relative_to(work_directory)),
            "--seed=1",
            args.package_path,
            "--",
        ]
        help_result = run_command(run_generated + ["--help"], work_directory)
        if help_result.returncode != 0:
            raise AssertionError(help_result.stdout + help_result.stderr)
        for expected in ["--output-dir DIRECTORY", "(default: .)", "--overwrite"]:
            if expected not in help_result.stdout:
                raise AssertionError(f"generated help omitted {expected!r}")

        output_directory = pathlib.Path("standalone") / "nested"
        standalone = run_generated + [
            "--output-dir",
            str(output_directory),
        ]
        first = run_command(standalone, work_directory)
        if first.returncode != 0:
            raise AssertionError(first.stdout + first.stderr)
        if "Beginning MCMC computations." not in first.stdout:
            raise AssertionError("the standalone program did not report the start of execution")

        output_paths = [work_directory / output_directory / name for name in ["C1.log", "C1.trees"]]
        if not all(path.is_file() for path in output_paths):
            raise AssertionError(f"the standalone program did not create its log files: {output_paths}")
        if (work_directory / output_directory / "C1.run.json").exists():
            raise AssertionError("the standalone program unexpectedly created the C++-owned run manifest")

        original_contents = {path: path.read_bytes() for path in output_paths}
        collision = run_command(standalone, work_directory)
        if collision.returncode == 0:
            raise AssertionError("a standalone rerun overwrote output files without --overwrite")
        if "Refusing to overwrite existing BAli-Phy output files" not in collision.stderr:
            raise AssertionError(collision.stdout + collision.stderr)
        if any(path.read_bytes() != contents for path, contents in original_contents.items()):
            raise AssertionError("collision handling modified an existing output file")

        overwritten = run_command(standalone + ["--overwrite"], work_directory)
        if overwritten.returncode != 0:
            raise AssertionError(overwritten.stdout + overwritten.stderr)

    return 0


if __name__ == "__main__":
    sys.exit(main())
