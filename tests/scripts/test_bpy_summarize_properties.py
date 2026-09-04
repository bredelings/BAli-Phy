#!/usr/bin/env python3

from contextlib import redirect_stdout
import io
import json
import os
from pathlib import Path
import runpy
import tempfile
import time
import unittest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts" / "bpy-summarize"
MODULE = runpy.run_path(str(SCRIPT))
Analysis = MODULE["Analysis"]
BAliPhyRun = MODULE["BAliPhyRun"]


class FakeRun:
    # Expose one optional character-property log per partition.
    def __init__(self, files):
        self.files = files

    def get_character_property_files(self):
        return self.files

    def n_partitions(self):
        return len(self.files)


class BPYSummarizePropertyTests(unittest.TestCase):
    # Construct only the analysis state needed by the independently testable seams.
    def make_analysis(self, directory, partition_files):
        analysis = Analysis.__new__(Analysis)
        analysis.outdir = Path(directory)
        analysis.mcmc_runs = [FakeRun(files) for files in partition_files]
        analysis.burnin = 10
        analysis.until = 80
        analysis.subsample = 3
        analysis.verbose = False
        return analysis

    # Discover exact one-based logger filenames without inventing absent streams.
    def test_discovers_partition_property_logs(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            samples = directory / "C1.P1.site-property-samples.jsonl"
            samples.write_text("", encoding="utf-8")
            run = BAliPhyRun.__new__(BAliPhyRun)
            run.dir = directory
            run.input_files = ["one.fasta", "two.fasta"]
            run.character_property_files = run.find_character_property_files()

            self.assertEqual(
                run.get_character_property_files(),
                [samples, None],
            )

    # Generate one pooled summary, forward selection options, and honor timestamps.
    def test_summarizes_all_chains_and_reuses_fresh_output(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            raw_files = [directory / "chain-1.json", directory / "chain-2.json"]
            for filename in raw_files:
                filename.write_text("{}\n", encoding="utf-8")
            analysis = self.make_analysis(directory, [[raw_files[0]], [raw_files[1]]])
            commands = []

            # Capture commands while materializing the declared output artifact.
            def execute(command, **kwargs):
                commands.append((command, kwargs["outfile"]))
                kwargs["outfile"].write_text("{}\n", encoding="utf-8")

            analysis.exec_show = execute
            analysis.summarize_character_properties()

            output = directory / "P1.site-property-summary.json"
            self.assertEqual(
                commands[0],
                (
                    [
                        "character-properties",
                        "summarize",
                        *raw_files,
                        "--skip=10",
                        "--until=80",
                        "--subsample=3",
                    ],
                    output,
                ),
            )
            self.assertEqual(analysis.character_property_summaries, [output])

            newest_input = max(filename.stat().st_mtime for filename in raw_files)
            os.utime(output, (newest_input + 10, newest_input + 10))
            analysis.summarize_character_properties()
            self.assertEqual(len(commands), 1)

            os.utime(raw_files[0], (newest_input + 20, newest_input + 20))
            analysis.summarize_character_properties()
            self.assertEqual(len(commands), 2)

    # Keep paired site ranking in character-properties while preserving both posterior counts and
    # condition states. Regenerate derived TSV tables so their columns always match the current
    # parser; character-properties formatting tests do not cover this orchestration.
    def test_generates_positive_selection_reports(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            summary = directory / "P1.site-property-summary.json"
            alignment = directory / "P1.initial.fasta"
            summary.write_text(json.dumps({
                "retained_samples": 100,
                "properties": {
                    "background-posSelection": {},
                    "background-dNdS": {},
                    "posSelection": {},
                    "dNdS": {},
                },
                "conditioned": {
                    "positiveSelectionInModel": {
                        "retained_samples": 60,
                        "properties": {"posSelection": {}, "dNdS": {}},
                    },
                },
            }), encoding="utf-8")
            alignment.write_text(">A\nAAA\n", encoding="utf-8")

            analysis = self.make_analysis(directory, [])
            analysis.character_property_summaries = [summary]
            commands = []

            # Materialize a small valid table for each requested property report.
            def execute(command, **kwargs):
                commands.append(command)
                conditioned_probability = "" if "--unconditional" in command else "0.75"
                conditioned_dnds = "\t\t" if "--unconditional" in command else "2.5\t0.4\t2.4"
                kwargs["outfile"].write_text(
                    "column\tsequence\tsequence-character\tsymbol\ttranslation\t"
                    "model-averaged-probability\tmodel-averaged-dNdS-mean\tmodel-averaged-dNdS-sd\t"
                    "model-averaged-dNdS-median\tconditioned-probability\tconditioned-dNdS-mean\t"
                    "conditioned-dNdS-sd\tconditioned-dNdS-median\n"
                    f"1\tA\t1\tAAA\tK\t0.25\t1.5\t0.2\t1.4\t{conditioned_probability}\t"
                    f"{conditioned_dnds}\n",
                    encoding="utf-8",
                )

            analysis.exec_show = execute
            analysis.summarize_positive_selection()

            self.assertEqual(len(commands), 2)
            background_command, conditioned_command = commands
            self.assertEqual(background_command[4], "background-posSelection")
            self.assertIn("--unconditional", background_command)
            self.assertEqual(conditioned_command[4], "posSelection")
            self.assertNotIn("--unconditional", conditioned_command)
            self.assertEqual(
                [report["filename"].name for report in analysis.positive_selection_reports],
                ["P1.background-positive-selection.tsv", "P1.positive-selection.tsv"],
            )
            self.assertEqual(analysis.positive_selection_reports[0]["retained_samples"], 100)
            self.assertEqual(analysis.positive_selection_reports[1]["retained_samples"], 60)
            self.assertEqual(analysis.positive_selection_reports[1]["total_samples"], 100)
            self.assertEqual(analysis.positive_selection_reports[1]["model_averaged_count"], 0)
            self.assertEqual(analysis.positive_selection_reports[1]["conditioned_count"], 1)
            self.assertEqual(analysis.positive_selection_reports[1]["rows"][0]["symbol"], "AAA")

            stale_output = directory / "P1.positive-selection.tsv"
            stale_output.write_text("stale\n", encoding="utf-8")
            newest_input = max(summary.stat().st_mtime, alignment.stat().st_mtime)
            os.utime(stale_output, (newest_input + 10, newest_input + 10))
            analysis.summarize_positive_selection()
            self.assertEqual(commands[2:], commands[:2])
            self.assertTrue(stale_output.read_text(encoding="utf-8").startswith("column\t"))
            self.assertEqual(analysis.positive_selection_reports[1]["conditioned_count"], 1)

            analysis.get_column_name_map = lambda: {
                "S1/M3_test:LogOddsPosSelection": "M3_test:LogOddsPosSelection",
            }
            analysis.get_smodel_indices = lambda: [0]
            analysis.positive_selection_statistics = {
                "M3_test:LogOddsPosSelection": {
                    "posterior_probability": "0.5702",
                    "log_odds": "0.2825",
                },
            }
            (directory / "P1.initial.html").write_text("", encoding="utf-8")
            section = analysis.section_positive_selection()
            self.assertIn('<h3 id="positive-selection-P1">', section)
            self.assertNotIn('class="anchor"', section)
            self.assertIn('href="#positive-selection-P1">P1</a>', section)
            self.assertIn('href="#S1">S1</a>', section)
            self.assertIn("The overall posterior allows positive selection to be absent", section)
            self.assertNotIn("Pr(positive selection is in the model):", section)
            self.assertNotIn("Positive selection is included in this model.", section)
            self.assertIn(
                "Columns with Pr(dN/dS &gt; 1) &gt; 0.5: 0 overall, 1 with selection",
                section,
            )
            overall_heading = section.index("Overall posterior</th>")
            selection_heading = section.index("Posterior with selection</th>")
            self.assertLess(overall_heading, selection_heading)
            self.assertIn('<td class="site-probability">0.250</td>', section)
            self.assertIn('<td class="site-probability">0.750</td>', section)
            self.assertIn('<td class="site-dnds-pm">&plusmn;</td>', section)
            self.assertIn('href="P1.positive-selection.tsv">Complete TSV table</a>', section)
            self.assertIn('href="P1.initial.html">Alignment viewer</a>', section)

            unconditional_report = analysis.positive_selection_reports[0]
            paired_report = analysis.positive_selection_reports[1]
            many_rows_report = {
                **paired_report,
                "rows": [
                    {**paired_report["rows"][0], "column": str(column)}
                    for column in range(1, 26)
                ],
            }
            analysis.positive_selection_reports = [many_rows_report]
            many_rows_section = analysis.section_positive_selection()
            self.assertIn(
                "Showing 20 of 25 selected columns, ordered by overall posterior probability",
                many_rows_section,
            )
            self.assertEqual(many_rows_section.count('class="site-column"'), 20)

            analysis.positive_selection_reports = [paired_report]
            paired_report["retained_samples"] = 100
            paired_report["model_averaged_count"] = 1
            all_true_section = analysis.section_positive_selection()
            self.assertNotIn("Pr(positive selection is in the model):", all_true_section)
            self.assertNotIn("Positive selection is included in this model.", all_true_section)
            self.assertNotIn("The overall posterior allows positive selection to be absent", all_true_section)
            self.assertIn("Columns with Pr(dN/dS &gt; 1) &gt; 0.5: 1</p>", all_true_section)
            self.assertIn("Overall posterior</th>", all_true_section)
            self.assertNotIn("Posterior with selection</th>", all_true_section)

            paired_report["retained_samples"] = 0
            paired_report["model_averaged_count"] = 0
            paired_report["conditioned_count"] = 0
            paired_report["rows"] = []
            zero_true_section = analysis.section_positive_selection()
            self.assertNotIn("Pr(positive selection is in the model):", zero_true_section)
            self.assertNotIn("Positive selection is not included in this model.", zero_true_section)
            self.assertIn("Columns with Pr(dN/dS &gt; 1) &gt; 0.5: 0 overall", zero_true_section)

            analysis.positive_selection_reports = [unconditional_report]
            unconditional_section = analysis.section_positive_selection()
            self.assertNotIn("Pr(positive selection is in the model):", unconditional_section)
            self.assertIn("Columns with Pr(dN/dS &gt; 1) &gt; 0.5: 0 overall", unconditional_section)
            self.assertIn("Overall posterior</th>", unconditional_section)
            self.assertNotIn("Posterior with selection</th>", unconditional_section)

            analysis.get_column_name_map = lambda: {"S1/M2a:posW": "M2a:posW"}
            analysis.positive_selection_reports = [paired_report]
            paired_report["retained_samples"] = 100
            paired_report["total_samples"] = 100
            paired_report["model_averaged_count"] = 1
            paired_report["rows"] = unconditional_report["rows"]
            fixed_on_section = analysis.section_positive_selection()
            self.assertIn("Positive selection is included in this model.", fixed_on_section)

            paired_report["retained_samples"] = 0
            paired_report["model_averaged_count"] = 0
            paired_report["rows"] = []
            fixed_off_section = analysis.section_positive_selection()
            self.assertIn("Positive selection is not included in this model.", fixed_off_section)

            analysis.get_column_name_map = lambda: None
            unknown_section = analysis.section_positive_selection()
            self.assertNotIn("Positive selection is not included in this model.", unknown_section)

            summary_data = json.loads(summary.read_text(encoding="utf-8"))
            summary_data["conditioned"]["positiveSelectionInModel"] = {
                "retained_samples": 0,
                "properties": {},
            }
            summary.write_text(json.dumps(summary_data), encoding="utf-8")
            for output in directory.glob("P1.*positive-selection.tsv"):
                output.unlink()
            commands.clear()
            analysis.summarize_positive_selection()
            self.assertTrue(all("--unconditional" not in command for command in commands))
            self.assertTrue(all(report["retained_samples"] == 0 for report in analysis.positive_selection_reports))

    # Refuse to pool a partition when only some chains logged properties.
    def test_skips_partial_chain_property_logs(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            raw_file = directory / "chain-1.json"
            raw_file.write_text("{}\n", encoding="utf-8")
            analysis = self.make_analysis(directory, [[raw_file], [None]])
            commands = []
            analysis.exec_show = lambda command, **kwargs: commands.append(command)

            messages = io.StringIO()
            with redirect_stdout(messages):
                analysis.summarize_character_properties()

            self.assertEqual(commands, [])
            self.assertEqual(analysis.character_property_summaries, [None])
            self.assertIn("all chains", messages.getvalue())

    # Pair property data with its alphabet and exclude ancestral sequence pages.
    def test_maps_tip_alignments_and_forms_combined_draw_command(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            summary = directory / "P1.site-property-summary.json"
            analysis = self.make_analysis(directory, [])
            analysis.character_property_summaries = [summary]

            self.assertEqual(analysis.character_properties_for_alignment("P1.initial"), summary)
            self.assertEqual(analysis.character_properties_for_alignment("P1.consensus.pd-wsum"), summary)
            self.assertIsNone(analysis.character_properties_for_alignment("P1.ancestors"))
            self.assertIsNone(analysis.character_properties_for_alignment("P1.initial-diff"))
            self.assertIsNone(analysis.character_properties_for_alignment("MAP"))

            commands = []
            analysis.exec_show = lambda command, **kwargs: commands.append((command, kwargs))
            alignment = directory / "P1.initial.fasta"
            uncertainty = directory / "P1.initial-AU.prob"
            output = directory / "P1.initial-AU.html"
            analysis.draw_alignment(
                alignment,
                outfile=output,
                properties=summary,
                alphabet="DNA",
                AU=uncertainty,
            )

            self.assertEqual(
                commands[0][0],
                [
                    "alignment-draw",
                    alignment,
                    "--AU",
                    uncertainty,
                    "--properties",
                    summary,
                    "--alphabet",
                    "DNA",
                ],
            )
            self.assertEqual(commands[0][1]["outfile"], output)

    # Pass the partition alphabet even when an alignment has no property summary.
    def test_draw_alignment_uses_alphabet_without_properties(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            analysis = self.make_analysis(directory, [])
            commands = []
            analysis.exec_show = lambda command, **kwargs: commands.append(command)

            analysis.draw_alignment(directory / "P1.initial.fasta", alphabet="Codons(DNA,mt-vert)")

            self.assertEqual(
                commands[0],
                [
                    "alignment-draw",
                    directory / "P1.initial.fasta",
                    "--alphabet",
                    "Codons(DNA,mt-vert)",
                ],
            )

    # Keep codon AU generation and the combined viewer on the same alphabet grid.
    def test_au_pipeline_forwards_alphabet_and_properties(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            alignment = directory / "P1.initial.fasta"
            raw_alignment = directory / "C1.P1.fastas"
            summary = directory / "P1.site-property-summary.json"
            map_tree = directory / "MAP.tree"
            for filename in (alignment, raw_alignment, summary, map_tree):
                filename.write_text("fixture\n", encoding="utf-8")

            analysis = self.make_analysis(directory, [])
            analysis.alignments = [("P1.initial", "Codons(DNA)", "Initial")]
            analysis.character_property_summaries = [summary]
            analysis.get_alignments_for_partition = lambda partition: [raw_alignment]
            commands = []

            # Capture both external commands and materialize their requested outputs.
            def execute(command, **kwargs):
                commands.append(command)
                if "outfile" in kwargs:
                    kwargs["outfile"].write_text("fixture\n", encoding="utf-8")

            analysis.exec_show = execute

            # Provide the two streaming pipeline objects without starting processes.
            class FakeProcess:
                def __init__(self, command, **kwargs):
                    self.command = command
                    self.stdout = object()

                def wait(self):
                    return 0

            original_popen = MODULE["subprocess"].Popen
            MODULE["subprocess"].Popen = FakeProcess
            try:
                analysis.compute_and_draw_AU_plots()
            finally:
                MODULE["subprocess"].Popen = original_popen

            gild_command = commands[0]
            draw_command = commands[1]
            self.assertIn("--alphabet", gild_command)
            self.assertEqual(gild_command[gild_command.index("--alphabet") + 1], "Codons(DNA)")
            self.assertIn("--properties", draw_command)
            self.assertEqual(draw_command[draw_command.index("--properties") + 1], summary)
            self.assertEqual(draw_command[draw_command.index("--alphabet") + 1], "Codons(DNA)")

            commands.clear()
            au_file = directory / "P1.initial-AU.prob"
            html_file = directory / "P1.initial-AU.html"
            base_time = time.time() - 1000
            for filename in (raw_alignment, map_tree, summary):
                os.utime(filename, (base_time, base_time))
            os.utime(au_file, (base_time + 10, base_time + 10))
            os.utime(alignment, (base_time + 20, base_time + 20))
            os.utime(html_file, (base_time + 30, base_time + 30))

            MODULE["subprocess"].Popen = FakeProcess
            try:
                analysis.compute_and_draw_AU_plots()
            finally:
                MODULE["subprocess"].Popen = original_popen
            self.assertEqual(commands[0][0], "alignment-gild")


if __name__ == "__main__":
    unittest.main()
