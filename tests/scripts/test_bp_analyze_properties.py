#!/usr/bin/env python3

from contextlib import redirect_stdout
import io
import os
from pathlib import Path
import runpy
import tempfile
import time
import unittest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts" / "bp-analyze"
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


class BPAnalyzePropertyTests(unittest.TestCase):
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
            (directory / "C1.properties1.json").write_text("", encoding="utf-8")
            run = BAliPhyRun.__new__(BAliPhyRun)
            run.dir = directory
            run.input_files = ["one.fasta", "two.fasta"]
            run.character_property_files = run.find_character_property_files()

            self.assertEqual(
                run.get_character_property_files(),
                [directory / "C1.properties1.json", None],
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

            output = directory / "P1.character-properties.json"
            self.assertEqual(
                commands[0],
                (["calc-properties", *raw_files, "--skip=10", "--until=80", "--subsample=3"], output),
            )
            self.assertEqual(analysis.character_property_summaries, [output])

            newest_input = max(filename.stat().st_mtime for filename in raw_files)
            os.utime(output, (newest_input + 10, newest_input + 10))
            analysis.summarize_character_properties()
            self.assertEqual(len(commands), 1)

            os.utime(raw_files[0], (newest_input + 20, newest_input + 20))
            analysis.summarize_character_properties()
            self.assertEqual(len(commands), 2)

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
            summary = directory / "P1.character-properties.json"
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

    # Keep codon AU generation and the combined viewer on the same alphabet grid.
    def test_au_pipeline_forwards_alphabet_and_properties(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            alignment = directory / "P1.initial.fasta"
            raw_alignment = directory / "C1.P1.fastas"
            summary = directory / "P1.character-properties.json"
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
