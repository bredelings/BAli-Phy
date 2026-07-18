#!/usr/bin/env python3

import argparse
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


class TreeMeanLengthsTests(unittest.TestCase):
    binary = None
    wrapper = []

    # Run tree-mean-lengths with temporary query and sample files.
    def run_tree_mean_lengths(self, samples, *options, stdin=""):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            query = directory / "query.tree"
            query.write_text("((A:1,B:1):1,C:1);\n", encoding="utf-8")

            filenames = []
            for index, contents in enumerate(samples):
                filename = directory / f"samples-{index}.trees"
                filename.write_text(contents, encoding="utf-8")
                filenames.append(str(filename))

            return subprocess.run(
                self.wrapper
                + [str(self.binary), f"--tree={query}", "--no-node-lengths", *options, *filenames],
                input=stdin,
                capture_output=True,
                check=False,
                text=True,
            )

    # Repeating an identical sample file must not alter finalized means or deviations.
    def test_finalizes_shared_accumulator_once(self):
        samples = "((A:1,B:2):3,C:4);\n((A:3,B:4):5,C:6);\n"
        once = self.run_tree_mean_lengths([samples])
        twice = self.run_tree_mean_lengths([samples, samples])
        self.assertEqual(once.returncode, 0, once.stderr)
        self.assertEqual(twice.returncode, 0, twice.stderr)
        self.assertEqual(twice.stdout, once.stdout)


if __name__ == "__main__":
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument("--wrapper", action="append", default=[])
    argument_parser.add_argument("binary", type=Path)
    arguments = argument_parser.parse_args()

    TreeMeanLengthsTests.binary = arguments.binary.resolve()
    TreeMeanLengthsTests.wrapper = arguments.wrapper
    unittest.main(argv=[sys.argv[0]])
