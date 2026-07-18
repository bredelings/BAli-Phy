#!/usr/bin/env python3

import argparse
from pathlib import Path
import re
import subprocess
import sys
import tempfile
import unittest


class StatreportTests(unittest.TestCase):
    binary = None
    wrapper = []

    # Run statreport on a non-monotone one-column trace containing the supplied values.
    def run_statreport(self, values, *options):
        with tempfile.TemporaryDirectory() as directory:
            filename = Path(directory) / "samples.tsv"
            rows = ["iteration\tvalue"]
            rows.extend(f"{index}\t{value}" for index, value in enumerate(values))
            filename.write_text("\n".join(rows) + "\n", encoding="utf-8")
            return subprocess.run(
                self.wrapper + [str(self.binary), "--skip=0", *options, str(filename)],
                capture_output=True,
                check=False,
                text=True,
            )

    # Extract the selected HPD endpoints from statreport's value summary.
    def hpd_interval(self, values):
        result = self.run_statreport(values, "--confidence=0.5")
        self.assertEqual(result.returncode, 0, result.stderr)
        match = re.search(r"value ~ .*\(([^,]+),([^\)]+)\) @ 50%", result.stdout)
        self.assertIsNotNone(match, result.stdout)
        return tuple(float(value) for value in match.groups())

    # Extract the mode estimate from statreport while suppressing its median report.
    def mode(self, values):
        result = self.run_statreport(values, "--mode", "--mean")
        self.assertEqual(result.returncode, 0, result.stderr)
        match = re.search(r"value \^ ([^ ]+)", result.stdout)
        self.assertIsNotNone(match, result.stdout)
        return float(match.group(1))

    # HPD selection examines unique optimal intervals at every significant scan position.
    def test_hpd_considers_first_interior_and_final_intervals(self):
        traces = [
            ([0] * 10 + list(range(20, 30)), (0, 0)),
            (list(range(5)) + [50] * 10 + list(range(96, 101)), (50, 50)),
            (list(range(10)) + [100] * 10, (100, 100)),
        ]
        for sorted_values, expected in traces:
            values = sorted_values[::2] + sorted_values[1::2]
            with self.subTest(expected=expected):
                self.assertEqual(self.hpd_interval(values), expected)

    # Mode selection also includes the final six-point window used for twenty samples.
    def test_mode_considers_first_interior_and_final_intervals(self):
        traces = [
            ([0] * 6 + list(range(20, 34)), 0),
            (list(range(7)) + [50] * 6 + list(range(94, 101)), 50),
            (list(range(14)) + [100] * 6, 100),
        ]
        for sorted_values, expected in traces:
            values = sorted_values[::2] + sorted_values[1::2]
            with self.subTest(expected=expected):
                self.assertEqual(self.mode(values), expected)


if __name__ == "__main__":
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument("--wrapper", action="append", default=[])
    argument_parser.add_argument("binary", type=Path)
    arguments = argument_parser.parse_args()

    StatreportTests.binary = arguments.binary.resolve()
    StatreportTests.wrapper = arguments.wrapper
    unittest.main(argv=[sys.argv[0]])
