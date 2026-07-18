#!/usr/bin/env python3

import argparse
from pathlib import Path
import subprocess
import sys
import unittest


class TreesDistancesTests(unittest.TestCase):
    binary = None
    wrapper = []

    # Run the matching metric on Newick trees and parse its distance matrix.
    def matching_matrix(self, trees):
        result = subprocess.run(
            self.wrapper + [str(self.binary), "matrix", "-", "--metric=matching"],
            input="\n".join(trees) + "\n",
            capture_output=True,
            check=False,
            text=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        return [[float(value) for value in row.split()] for row in result.stdout.splitlines()]

    # Dummy rows and columns use the real split from the opposite tree.
    def test_star_and_resolved_trees_in_both_orders(self):
        star = "(A,B,C,D);"
        resolved = "((A,B),(C,D));"
        expected = [[0.0, 2.0], [2.0, 0.0]]

        for trees in ([star, resolved], [resolved, star]):
            with self.subTest(trees=trees):
                self.assertEqual(self.matching_matrix(trees), expected)

    # Split costs are unchanged by orientation and count both sides of a crossing.
    def test_equal_complementary_and_crossing_splits(self):
        reference = "((A,B),(C,D));"
        cases = [
            ("equal", reference, 0.0),
            ("complementary", "((C,D),(A,B));", 0.0),
            ("crossing", "((A,C),(B,D));", 2.0),
        ]

        for name, other, expected in cases:
            with self.subTest(name=name):
                matrix = self.matching_matrix([reference, other])
                self.assertEqual(matrix, [[0.0, expected], [expected, 0.0]])


if __name__ == "__main__":
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument("--wrapper", action="append", default=[])
    argument_parser.add_argument("binary", type=Path)
    arguments = argument_parser.parse_args()

    TreesDistancesTests.binary = arguments.binary.resolve()
    TreesDistancesTests.wrapper = arguments.wrapper
    unittest.main(argv=[sys.argv[0]])
