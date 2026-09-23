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
        encoding="utf-8",
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

        # Generated multi-partition programs must diagnose name mismatches even if a fixed tree
        # makes taxa otherwise unused. Ordinary model tests do not cover that lazy-evaluation path.
        # The fixed-tree check remains necessary even if missing partition observations become supported.
        (work_directory / "second.fasta").write_text(">one\nACGT\n>three\nACGT\n", encoding="utf-8")
        (work_directory / "third.fasta").write_text(">two\nACGT\n>three\nACGT\n", encoding="utf-8")
        (work_directory / "tree.nwk").write_text("(one:0.1,two:0.1);\n", encoding="utf-8")
        for extra, expected in [
            ([], "Partition files must contain the same set of sequence names"),
            (["third.fasta", "--fix=tree=tree.nwk"],
             'Observation labels with no tree node'),
        ]:
            mismatch = run_command(args.wrapper + [
                args.executable, "--seed=1", args.package_path,
                "input.fasta", "second.fasta", *extra, "--imodel=none", "--test",
            ], work_directory)
            if mismatch.returncode == 0 or expected not in mismatch.stderr:
                raise AssertionError(mismatch.stdout + mismatch.stderr)

        # Check the generic association directly, including numeric internal-node observations.
        # This protects complete matching even when no generated analysis supplies validation.
        (work_directory / "observations.nwk").write_text("((one,two)ancestor,three);\n", encoding="utf-8")
        (work_directory / "Observations.hs").write_text("""{-# LANGUAGE NoImplicitPrelude #-}
import Prelude
import Bio.Alignment (observationsOnTree)
import Tree.Newick (readTreeTopology)
import qualified Data.Text as Text
import qualified Data.IntMap as IntMap
import Data.Maybe (catMaybes, isNothing)
import System.Environment (getArgs)
main = do
    tree <- readTreeTopology "observations.nwk"
    args <- getArgs
    let base = [(Text.pack n, v) | (n,v) <- [("one",1), ("two",2), ("three",3), ("ancestor",4)]]
        observations = case args of
            ["extra"] -> (Text.pack "absent", 5) : base
            ["missing"] -> tail base
            _ -> base
        values = IntMap.elems (observationsOnTree tree observations)
    print (sum (catMaybes values) :: Int, length (filter isNothing values))
""", encoding="utf-8")
        for mode, expected in [("valid", "(10,1)"),
                               ("extra", 'Observation labels with no tree node'),
                               ("missing", 'Tree-node labels with no observation')]:
            result = run_command(args.wrapper + [args.executable, args.package_path,
                                 "run", "Observations.hs", mode], work_directory)
            if (result.returncode == 0) != (mode == "valid") or expected not in result.stdout + result.stderr:
                raise AssertionError(result.stdout + result.stderr)

    return 0


if __name__ == "__main__":
    sys.exit(main())
