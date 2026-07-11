#!/usr/bin/env python3

import argparse
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class CutRangeTests(unittest.TestCase):
    binary = None
    wrapper = []

    # Run cut-range against temporary record files and return its completed process.
    def run_cut_range(self, contents, *options):
        with tempfile.TemporaryDirectory() as directory:
            filenames = []
            for index, content in enumerate(contents):
                filename = Path(directory) / f"samples-{index}.txt"
                filename.write_text(content, encoding="utf-8")
                filenames.append(str(filename))
            return subprocess.run(
                self.wrapper + [str(self.binary), *filenames, *options],
                capture_output=True,
                check=False,
                text=True,
            )

    # Selection preserves complete multi-line FASTA records while applying a stride.
    def test_filters_and_subsamples_complete_records(self):
        samples = "".join(
            f"iterations = {iteration}\n\n>A\nAC\n>B\nA-\n\n"
            for iteration in (0, 10, 20, 30, 40, 50)
        )
        result = self.run_cut_range(
            [samples], "--skip=0", "--until=40", "--subsample=2"
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            result.stdout,
            "iterations = 10\n\n>A\nAC\n>B\nA-\n\n"
            "iterations = 30\n\n>A\nAC\n>B\nA-\n\n",
        )

    # Each file starts a fresh stride so chains receive identical selection rules.
    def test_restarts_subsample_for_each_file(self):
        chain1 = "iterations = 10\na\niterations = 20\nb\n"
        chain2 = "iterations = 10\nc\niterations = 20\nd\n"
        result = self.run_cut_range([chain1, chain2], "--subsample=2")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "iterations = 10\na\niterations = 10\nc\n")

    # Invalid strides should fail before reading or partially emitting samples.
    def test_rejects_nonpositive_subsample(self):
        result = self.run_cut_range(["iterations = 0\na\n"], "--subsample=0")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("subsample", result.stderr)


if __name__ == "__main__":
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument("--wrapper", action="append", default=[])
    argument_parser.add_argument("binary", type=Path)
    arguments = argument_parser.parse_args()

    CutRangeTests.binary = arguments.binary.resolve()
    CutRangeTests.wrapper = arguments.wrapper
    unittest.main(argv=[sys.argv[0]])
