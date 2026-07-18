#!/usr/bin/env python3

import argparse
from pathlib import Path
import subprocess
import sys
import unittest


class AlignmentConvertTests(unittest.TestCase):
    binary = None
    wrapper = []

    # Complete final PHYLIP lines are accepted regardless of whether they end in a newline.
    def test_phylip_with_and_without_terminal_newline(self):
        inputs = {
            "sequential": "2 4\nTaxon1    AACC\nTaxon2    AACC",
            "interleaved": "2 4\nTaxon1    AA\nTaxon2    AA\n\nCC\nCC",
        }
        expected = ">Taxon1   \nAACC\n>Taxon2   \nAACC\n\n"

        for layout, contents in inputs.items():
            for terminal_newline in (False, True):
                with self.subTest(layout=layout, terminal_newline=terminal_newline):
                    result = subprocess.run(
                        self.wrapper + [str(self.binary), "--output=fasta"],
                        input=contents + ("\n" if terminal_newline else ""),
                        capture_output=True,
                        check=False,
                        text=True,
                    )
                    self.assertEqual(result.returncode, 0, result.stderr)
                    self.assertEqual(result.stdout, expected)


if __name__ == "__main__":
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument("--wrapper", action="append", default=[])
    argument_parser.add_argument("binary", type=Path)
    arguments = argument_parser.parse_args()

    AlignmentConvertTests.binary = arguments.binary.resolve()
    AlignmentConvertTests.wrapper = arguments.wrapper
    unittest.main(argv=[sys.argv[0]])
