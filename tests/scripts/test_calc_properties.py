#!/usr/bin/env python3

import json
import math
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


CALC_PROPERTIES = Path(__file__).resolve().parents[2] / "scripts" / "calc-properties"


class CalcPropertiesTests(unittest.TestCase):
    # Write each chain as JSON Lines and run the public command-line interface.
    def run_calculator(self, chains, *options):
        with tempfile.TemporaryDirectory() as directory:
            filenames = []
            for index, samples in enumerate(chains, start=1):
                filename = Path(directory) / f"chain-{index}.jsonl"
                lines = [json.dumps(sample, separators=(",", ":")) for sample in samples]
                filename.write_text("\n".join(lines) + "\n", encoding="utf-8")
                filenames.append(str(filename))

            command = [sys.executable, str(CALC_PROPERTIES), *filenames, *options]
            return subprocess.run(
                command,
                capture_output=True,
                check=False,
                text=True,
                timeout=10,
            )

    # Require successful execution and decode the single JSON result document.
    def assert_calculator_succeeds(self, result):
        self.assertEqual(result.returncode, 0, msg=result.stderr)
        try:
            return json.loads(result.stdout)
        except json.JSONDecodeError as error:
            self.fail(f"calc-properties did not write valid JSON: {error}\n{result.stdout}")

    # Require a concise contextual validation error rather than a Python traceback.
    def assert_validation_error(self, result, *phrases):
        self.assertNotEqual(result.returncode, 0)
        self.assertNotIn("Traceback", result.stderr)
        for phrase in phrases:
            self.assertIn(phrase, result.stderr)

    # Check sample-specific property lookup and exact pooled means and counts.
    def test_computes_exact_means_and_counts(self):
        samples = [
            {
                "iter": 0,
                "catStates": {"A": [[0, 0], [1, 0]], "B": [[0, 1]]},
                "properties": {
                    "rate": [[1.0, 3.0], [10.0]],
                    "score": [[-1.0, 2.0], [4.0]],
                },
            },
            {
                "iter": 10,
                "catStates": {"A": [[0, 1], [1, 0]], "B": [[0, 0]]},
                "properties": {
                    "rate": [[2.0, 6.0], [20.0]],
                    "score": [[1.0, 5.0], [8.0]],
                },
            },
        ]

        result = self.run_calculator([samples])
        output = self.assert_calculator_succeeds(result)

        self.assertEqual(output["retained_samples"], 2)
        self.assertEqual(output["retained_samples_by_chain"], [2])
        self.assertEqual(
            output["properties"]["rate"]["mean"],
            {"A": [3.5, 15.0], "B": [2.5]},
        )
        self.assertEqual(
            output["properties"]["rate"]["count"],
            {"A": [2, 2], "B": [2]},
        )
        self.assertEqual(
            output["properties"]["score"]["mean"],
            {"A": [2.0, 6.0], "B": [1.5]},
        )
        self.assertEqual(
            output["properties"]["score"]["count"],
            {"A": [2, 2], "B": [2]},
        )

    # Check strict skip, inclusive until, and post-filter per-chain stride semantics.
    def test_filters_samples_by_iteration_and_subsample(self):
        samples = [
            {
                "iter": iteration,
                "catStates": {"A": [[0, 0]]},
                "properties": {"rate": [[float(iteration)]]},
            }
            for iteration in (0, 10, 20, 30)
        ]

        result = self.run_calculator(
            [samples],
            "--skip=0",
            "--until=30",
            "--subsample=2",
        )
        output = self.assert_calculator_succeeds(result)

        self.assertEqual(output["retained_samples"], 2)
        self.assertEqual(output["properties"]["rate"]["mean"], {"A": [20.0]})
        self.assertEqual(output["properties"]["rate"]["count"], {"A": [2]})

    # Check that chains are pooled by retained draw rather than by chain mean.
    def test_pools_multiple_chains_by_sample(self):
        chain1 = [
            {
                "iter": 0,
                "catStates": {"A": [[0, 0]]},
                "properties": {"rate": [[value]]},
            }
            for value in (1.0, 3.0)
        ]
        chain1[1]["iter"] = 10
        chain2 = [
            {
                "iter": 0,
                "catStates": {"A": [[0, 0]]},
                "properties": {"rate": [[9.0]]},
            }
        ]

        result = self.run_calculator([chain1, chain2])
        output = self.assert_calculator_succeeds(result)

        self.assertEqual(output["retained_samples"], 3)
        self.assertEqual(output["retained_samples_by_chain"], [2, 1])
        self.assertAlmostEqual(output["properties"]["rate"]["mean"]["A"][0], 13.0 / 3.0)
        self.assertEqual(output["properties"]["rate"]["count"], {"A": [3]})

    # Check that unavailable draws are omitted and represented by per-cell counts.
    def test_averages_available_values_and_preserves_all_missing_cells(self):
        samples = [
            {
                "iter": 0,
                "catStates": {"A": [None, None]},
                "properties": {"rate": [[2.0, 4.0]]},
            },
            {
                "iter": 10,
                "catStates": {"A": [[0, 0], None]},
                "properties": {"rate": [[2.0, 4.0]]},
            },
            {
                "iter": 20,
                "catStates": {"A": [[0, 1], None]},
                "properties": {"rate": [[2.0, 4.0]]},
            },
        ]

        result = self.run_calculator([samples])
        output = self.assert_calculator_succeeds(result)

        self.assertEqual(output["retained_samples"], 3)
        self.assertEqual(output["properties"]["rate"]["mean"], {"A": [3.0, None]})
        self.assertEqual(output["properties"]["rate"]["count"], {"A": [2, 0]})

    # Reject changing character counts instead of silently truncating with zip().
    def test_rejects_changed_sequence_lengths(self):
        samples = [
            {
                "iter": 0,
                "catStates": {"A": [[0, 0], [0, 0]]},
                "properties": {"rate": [[1.0]]},
            },
            {
                "iter": 10,
                "catStates": {"A": [[0, 0]]},
                "properties": {"rate": [[1.0]]},
            },
        ]

        result = self.run_calculator([samples])

        self.assert_validation_error(result, "chain-1.jsonl:2", "sequence", "length")

    # Reject negative category indices, which Python would otherwise index from the end.
    def test_rejects_negative_component_indices(self):
        samples = [
            {
                "iter": 0,
                "catStates": {"A": [[-1, 0]]},
                "properties": {"rate": [[1.0], [2.0]]},
            }
        ]

        result = self.run_calculator([samples])

        self.assert_validation_error(result, "chain-1.jsonl:1", "A", "character 0")

    # Reject property definitions that change between samples in one chain.
    def test_rejects_changed_property_names(self):
        samples = [
            {
                "iter": 0,
                "catStates": {"A": [[0, 0]]},
                "properties": {"rate": [[1.0]]},
            },
            {
                "iter": 10,
                "catStates": {"A": [[0, 0]]},
                "properties": {"score": [[1.0]]},
            },
        ]

        result = self.run_calculator([samples])

        self.assert_validation_error(result, "chain-1.jsonl:2", "property")

    # Reject non-finite values so the summary remains strict browser-readable JSON.
    def test_rejects_nonfinite_property_values(self):
        samples = [
            {
                "iter": 0,
                "catStates": {"A": [[0, 0]]},
                "properties": {"rate": [[math.nan]]},
            }
        ]

        result = self.run_calculator([samples])

        self.assert_validation_error(result, "chain-1.jsonl:1", "rate", "finite")


if __name__ == "__main__":
    unittest.main()
