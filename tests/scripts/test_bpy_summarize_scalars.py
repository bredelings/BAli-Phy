#!/usr/bin/env python3

from contextlib import redirect_stdout
import io
import os
from pathlib import Path
import runpy
import tempfile
from types import SimpleNamespace
import unittest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts" / "bpy-summarize"
MODULE = runpy.run_path(str(SCRIPT))
Analysis = MODULE["Analysis"]
TreeFileRun = MODULE["TreeFileRun"]
TreeLogFileRun = MODULE["TreeLogFileRun"]
print_model = MODULE["print_model"]
print_model_string = MODULE["print_model_string"]


class BPYSummarizeRunTests(unittest.TestCase):
    # Analysis-level format decisions must reflect the common run personality; direct scalar HTML
    # tests otherwise avoid format-specific columns. Remove this if formats get separate renderers.
    def test_uses_the_common_run_personality(self):
        analysis = Analysis.__new__(Analysis)
        analysis.mcmc_runs = [SimpleNamespace(personality=lambda: "phylobayes") for _ in range(2)]
        self.assertEqual(analysis.personality(), "phylobayes")

    # Plain tree samples have no header and every line is an iteration; broader report tests do not
    # cover a one-tree run. Remove this if tree counts come from parsed sample metadata instead.
    def test_counts_every_plain_tree_line(self):
        with tempfile.TemporaryDirectory() as directory:
            trees = Path(directory) / "chain.trees"
            trees.write_text("(a,b);\n", encoding="utf-8")
            self.assertEqual(TreeFileRun(trees).n_iterations(), 1)

    # Paired prefix inputs must resolve to distinct tree and scalar files; broader report tests use
    # BAli-Phy directories instead. Remove this if run discovery is replaced by explicit file arguments.
    def test_resolves_paired_tree_and_log_prefix(self):
        with tempfile.TemporaryDirectory() as directory:
            prefix = Path(directory) / "chain"
            prefix.with_suffix(".trees").write_text("(a,b);\n", encoding="utf-8")
            prefix.with_suffix(".log").write_text("iter\tvalue\n0\t1\n", encoding="utf-8")

            run = TreeLogFileRun(prefix)

            self.assertEqual(run.get_trees_file(), prefix.with_suffix(".trees"))
            self.assertEqual(run.get_log_file(), prefix.with_suffix(".log"))
            self.assertEqual(run.n_iterations(), 1)

    # Mean-length outputs belong to the selected report directory, and one cached level must not
    # hide a later missing level. Remove this if one command generates all consensus levels.
    def test_updates_each_stale_mean_branch_length_in_the_output_directory(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            trees = directory / "chain.trees"
            trees.write_text("(a,b);\n", encoding="utf-8")
            current = directory / "c50.ltree"
            current.write_text("(a,b);\n", encoding="utf-8")
            current_time = trees.stat().st_mtime + 10
            os.utime(current, (current_time, current_time))

            calls = []
            analysis = Analysis.__new__(Analysis)
            analysis.outdir = directory
            analysis.tree_consensus_levels = [0.5, 0.66]
            analysis.prune = None
            analysis.subsample = 1
            analysis.burnin = 0
            analysis.until = None
            analysis.get_trees_files = lambda: [trees]
            analysis.exec_show = lambda command, **kwargs: calls.append((command, kwargs))

            with redirect_stdout(io.StringIO()):
                analysis.compute_mean_branch_lengths()

        self.assertEqual(len(calls), 1)
        self.assertIn(directory / "c66.tree", calls[0][0])
        self.assertEqual(calls[0][1]["outfile"], directory / "c66.ltree")

    # A reusable decoding intermediate must not suppress reconstruction of its ordered report file;
    # end-to-end tests normally create both together. Remove this if the intermediate is eliminated.
    def test_orders_cached_posterior_decoding_alignments(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            work = directory / "Work"
            work.mkdir()
            samples = directory / "samples.fastas"
            samples.write_text("sample\n", encoding="utf-8")

            ordered = []
            analysis = Analysis.__new__(Analysis)
            analysis.outdir = directory
            analysis.alignments = []
            analysis.n_partitions = lambda: 1
            analysis.get_imodel_for_partition = lambda partition: "model"
            analysis.get_alignments_for_partition = lambda partition: [samples]
            analysis.get_alphabets = lambda: ["DNA"]
            analysis.make_ordered_alignment = ordered.append
            analysis.exec_show = lambda *args, **kwargs: self.fail("unexpected posterior decoding")

            for score in MODULE["scores"]:
                unordered = work / f"P1.consensus.pd-{score}-unordered.fasta"
                unordered.write_text("decoded\n", encoding="utf-8")
                output_time = samples.stat().st_mtime + 10
                os.utime(unordered, (output_time, output_time))

            with redirect_stdout(io.StringIO()):
                analysis.compute_wpd_alignments()

        self.assertEqual(ordered, [f"P1.consensus.pd-{score}" for score in MODULE["scores"]])


class BPYSummarizeScalarTests(unittest.TestCase):
    # Keep statreport's sampled and constant formats distinct and preserve aligned paired summaries.
    # This can be removed if bpy-summarize stops parsing and presenting statreport's text output.
    def test_parses_and_formats_scalar_summaries(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            log_file = directory / "C1.log"
            log_file.write_text("iter\tparameter\n0\t0\n", encoding="utf-8")
            report_file = directory / "Report"
            report_file.write_text(
                """\
parameter ~ 0.1  (0.0,0.2) @ 95%
          t @ 1   Ne = 10   burnin = 0

""",
                encoding="utf-8",
            )
            report_time = log_file.stat().st_mtime + 10
            os.utime(report_file, (report_time, report_time))

            commands = []

            # Capture the refreshed command and replace the obsolete report with its expected output.
            def execute(command, **kwargs):
                commands.append(command)
                kwargs["outfile"].write_text(
                    """\
M3_test:LogOddsPosSelection:  posterior-probability = 0.5702  [log-odds = 0.2825]
E M3_test:LogOddsPosSelection = 0.1782  [+- 0.5034]
M3_test:LogOddsPosSelection ~ 0.1548  (NA,NA) @ 95%
                              t @ 21.24   Ne = 4   burnin = Not Converged!

constant = 3
trend = [increasing]
""",
                    encoding="utf-8",
                )

            analysis = Analysis.__new__(Analysis)
            analysis.outdir = directory
            analysis.get_log_files = lambda: [log_file]
            analysis.subsample = None
            analysis.burnin = None
            analysis.until = None
            analysis.exec_show = execute
            analysis.get_column_name_map = lambda: {
                "iter": "iter",
                "S2/M3_test:LogOddsPosSelection": "M3_test:LogOddsPosSelection",
            }
            analysis.get_smodel_indices = lambda: [1, 0, 1]

            with redirect_stdout(io.StringIO()):
                analysis.summarize_numerical_parameters()

            self.assertEqual(commands, [["statreport", "--mean", "--median", log_file]])
            self.assertEqual(analysis.mean["M3_test:LogOddsPosSelection"], "0.1782")
            self.assertEqual(analysis.stddev["M3_test:LogOddsPosSelection"], "0.5034")
            self.assertEqual(analysis.median["M3_test:LogOddsPosSelection"], "0.1548")
            self.assertEqual(analysis.median["constant"], "3")
            self.assertEqual(analysis.constants, {"constant"})
            self.assertEqual(analysis.median["trend"], "[increasing]")
            self.assertNotIn("posterior-probability = 0.5702  [log-odds", analysis.median)
            self.assertEqual(
                analysis.positive_selection_model_support(),
                [{
                    "statistic": "M3_test:LogOddsPosSelection",
                    "smodel": 2,
                    "partitions": [1, 3],
                    "posterior_probability": "0.5702",
                    "log_odds": "0.2825",
                }],
            )

            variable = "M3_test:LogOddsPosSelection"
            analysis.ESS[variable] = 150
            analysis.PSRF_CI80[variable] = 1.1
            analysis.PSRF_RCF[variable] = 1.2
            section = analysis.section_scalar_variables()
            self.assertIn('aria-label="Scalar variables table"', section)
            self.assertIn('<p class="diagnostic-legend">', section)
            self.assertIn('<table class="backlit2 scalar-variables">', section)
            self.assertIn("Mean &plusmn; SD", section)
            self.assertIn("Median (95% BCI)", section)
            self.assertIn('<th scope="colgroup" colspan="3">Mean &plusmn; SD</th>', section)
            self.assertIn('<th scope="colgroup" colspan="2">Median (95% BCI)</th>', section)
            self.assertIn('<td class="scalar-mean">0.1782</td>', section)
            self.assertIn('<td class="scalar-pm">&plusmn;</td>', section)
            self.assertIn('<td class="scalar-sd">0.5034</td>', section)
            self.assertIn('<td class="scalar-median">0.1548</td>', section)
            self.assertIn('<td class="scalar-bci">(NA, NA)</td>', section)
            self.assertIn('<td>21.24</td>', section)
            self.assertIn('class="diagnostic-caution"', section)
            self.assertIn('class="diagnostic-bad"', section)
            self.assertIn('<span class="visually-hidden">Caution: </span>150</td>', section)
            self.assertIn('<span class="visually-hidden">Caution: </span>1.1</td>', section)
            self.assertIn('<span class="visually-hidden">Concerning: </span>1.2</td>', section)
            self.assertIn(
                '<td class="diagnostic-bad"><span class="diagnostic-marker" aria-hidden="true">!</span>'
                '<span class="visually-hidden">Not converged</span></td>',
                section,
            )
            self.assertNotIn('Not Converged!</td>', section)
            self.assertNotIn('style="color:', section)
            self.assertIn('href="#glossary-act"', section)
            self.assertIn('href="#glossary-ess"', section)
            self.assertIn('href="#glossary-psrf-ci80"', section)
            self.assertIn('href="#glossary-psrf-rcf"', section)
            self.assertIn(
                """\
  <th scope="row">constant</th>
  <td class="scalar-mean">3</td>
  <td class="scalar-pm"></td>
  <td class="scalar-sd scalar-na">&mdash;</td>
  <td class="scalar-median">3</td>
  <td class="scalar-bci scalar-na">&mdash;</td>
  <td class="scalar-na">&mdash;</td>
  <td class="scalar-na">&mdash;</td>
  <td class="scalar-na">&mdash;</td>
  <td class="scalar-na">&mdash;</td>
  <td class="scalar-na">&mdash;</td>
""",
                section,
            )
            self.assertNotIn("mean-sd", section)
            self.assertNotIn("median-bci", section)
            table_body = section.split("<tbody>", 1)[1]
            for row in table_body.replace("<tr>", "<tr >").split("<tr ")[1:]:
                cells = row.split("</tr>", 1)[0]
                self.assertEqual(cells.count("<th") + cells.count("<td"), 11)

            header = analysis.html_header("scalar report")
            self.assertIn("table.scalar-variables td {text-align:right;}", header)
            self.assertIn("table.scalar-variables td.scalar-na {text-align:center;}", header)
            self.assertIn("td.scalar-pm {padding-left:0; padding-right:0; text-align:center;}", header)
            self.assertIn("table.model td.model-variable", header)
            self.assertIn(".phylogeny-grid", header)
            self.assertIn("display:grid", header)


class BPYSummarizeHtmlTests(unittest.TestCase):
    # Alignment summaries must keep informative-site counts distinct from percentages; generated
    # integration data can accidentally make them equal. Remove this if alignment-info becomes typed.
    def test_parses_informative_site_percentage(self):
        analysis = Analysis.__new__(Analysis)
        analysis.exec_show = lambda command: "sites: inform.: 7 (35%)\n"
        features = analysis.get_alignment_info(Path("alignment.fasta"))
        self.assertEqual(features["n_inform"], "7")
        self.assertEqual(features["p_inform"], "35")

    # Run details must show the integer number selected after until, burn-in, and stride; external
    # tools do not feed that count back to the renderer. Remove this if selection reports its count.
    def test_reports_the_selected_sample_count(self):
        # Supply only the run metadata used by the details table.
        def make_run(directory, iterations):
            return SimpleNamespace(
                get_command=lambda: None,
                get_version=lambda: None,
                get_parent_dir=lambda: directory.parent,
                get_dir=lambda: directory,
                n_iterations=lambda: iterations,
            )

        analysis = Analysis.__new__(Analysis)
        analysis.mcmc_runs = [make_run(Path("chain1"), 11), make_run(Path("chain2"), 6)]
        analysis.burnin = 2
        analysis.subsample = 3
        analysis.until = 10

        with redirect_stdout(io.StringIO()):
            section = analysis.section_analysis()

        rows = section.split("<tbody>", 1)[1].split("</tbody>", 1)[0].split("<tr>")[1:]
        self.assertIn("  <td>3</td>\n", rows[0])
        self.assertIn("  <td>2</td>\n", rows[1])
        self.assertNotIn(".0</td>", section)

    # Protect externally derived labels and run metadata from becoming report markup.
    # This can be removed if an escaping template engine takes ownership of HTML rendering.
    def test_escapes_dynamic_report_text(self):
        unsafe = 'value<&"quoted"'
        run = SimpleNamespace(
            get_command=lambda: unsafe,
            get_version=lambda: unsafe,
            get_parent_dir=lambda: Path(unsafe),
            get_dir=lambda: Path(unsafe),
            n_iterations=lambda: 10,
        )
        analysis = Analysis.__new__(Analysis)
        analysis.mcmc_runs = [run]
        analysis.burnin = 0
        analysis.subsample = 1
        analysis.until = None

        with redirect_stdout(io.StringIO()):
            section = analysis.section_analysis()

        escaped = "value&lt;&amp;&quot;quoted&quot;"
        self.assertIn(escaped, analysis.html_header(unsafe))
        self.assertIn(escaped, section)
        self.assertNotIn(unsafe, section)
        self.assertEqual(print_model_string(unsafe), f"= {escaped}")
        _, model = print_model({"main": "base", "extracted": [(unsafe, {"main": "nested", "extracted": []})]})
        self.assertIn(f'<td class="model-variable">{escaped}</td>', model)
        self.assertNotIn(f'<th scope="row">{escaped}</th>', model)

        svg = analysis.html_svg('plot<&".svg', unsafe, "90%")
        self.assertIn(f'role="img" aria-label="{escaped}"', svg)
        self.assertIn('href="plot&lt;&amp;&quot;.svg"', svg)
        self.assertIn(f">View {escaped}</a>", svg)

    # Protect source pages from malformed markup and network dependencies while preserving syntax
    # highlighting. This can be removed if a shared HTML renderer owns source-page generation.
    def test_creates_an_escaped_html5_source_page(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            source = directory / "model&.hs"
            output = directory / "model.html"
            source.write_text('value = "<&>"\n', encoding="utf-8")

            analysis = Analysis.__new__(Analysis)
            analysis.outdir = directory / "report"
            analysis.outdir.mkdir()
            analysis.libexecdir = directory / "libexec"
            installed_assets = analysis.libexecdir / "bpy-summarize-assets"
            installed_assets.mkdir(parents=True)
            asset_names = ("highlight.min.js", "haskell.min.js", "atom-one-light.min.css", "LICENSE")
            for filename in asset_names:
                (installed_assets / filename).write_text(filename, encoding="utf-8")
            analysis.get_code = lambda: []
            analysis.create_viewable_source(source, output)
            analysis.copy_code()
            page = output.read_text(encoding="utf-8")
            self.assertIn("<!DOCTYPE html>", page)
            self.assertIn('<html lang="en">', page)
            self.assertIn('<main>', page)
            self.assertIn("model&amp;.hs", page)
            self.assertIn('value = &quot;&lt;&amp;&gt;&quot;', page)
            self.assertIn('class="language-haskell"', page)
            self.assertIn('href="bpy-summarize-assets/atom-one-light.min.css"', page)
            self.assertIn('src="bpy-summarize-assets/highlight.min.js"', page)
            self.assertIn('src="bpy-summarize-assets/haskell.min.js"', page)
            self.assertNotIn("https://", page)
            for filename in asset_names:
                self.assertEqual((analysis.outdir / "bpy-summarize-assets" / filename).read_text(), filename)

    # Keep definitions out of reports that cannot contain the corresponding diagnostics.
    # This can be removed if glossary terms are generated directly from rendered columns.
    def test_builds_a_compact_conditional_glossary(self):
        analysis = Analysis.__new__(Analysis)
        analysis.has_parameters = lambda: True
        analysis.has_trees = lambda: True
        analysis.sampled_topology_count = 2

        glossary = analysis.section_glossary()
        for term in ("act", "ess", "psrf-ci80", "psrf-rcf", "asdsf", "msdsf"):
            self.assertIn(f'id="glossary-{term}"', glossary)

        analysis.has_parameters = lambda: False
        glossary = analysis.section_glossary()
        self.assertNotIn('id="glossary-act"', glossary)
        self.assertNotIn('id="glossary-psrf-ci80"', glossary)
        self.assertIn('id="glossary-ess"', glossary)
        self.assertIn('id="glossary-asdsf"', glossary)

        analysis.sampled_topology_count = 1
        self.assertEqual(analysis.section_glossary(), "")

        analysis.has_trees = lambda: False
        self.assertEqual(analysis.section_glossary(), "")

    # Keep plot captions associated with variable-topology plots and suppress those plots for a
    # fixed topology. Remove this if a separate renderer takes ownership of report figures.
    def test_wraps_report_plots_in_figures(self):
        with tempfile.TemporaryDirectory() as directory:
            analysis = Analysis.__new__(Analysis)
            analysis.outdir = Path(directory)
            analysis.trees = [("c50", "50% consensus")]
            analysis.subpartitions = False
            analysis.n_chains = lambda: 1
            analysis.has_trees = lambda: True
            analysis.R_exe = True
            analysis.sampled_topology_count = 2

            topology = analysis.section_phylogeny_distribution()
            mixing = analysis.section_tree_mixing2()

            analysis.sampled_topology_count = 1
            fixed_topology = analysis.section_phylogeny_distribution()
            analysis.has_parameters = lambda: False
            analysis.get_value_from_file = lambda *args: "NA"
            with redirect_stdout(io.StringIO()):
                fixed_mixing = analysis.section_mixing()

        self.assertEqual(topology.count('<figure class="plot-panel">'), 2)
        self.assertIn("<figcaption>50% consensus tree</figcaption>", topology)
        self.assertIn("<figcaption>Consensus-tree support levels</figcaption>", topology)
        self.assertIn("<caption>Tree files</caption>", topology)
        self.assertIn('<th scope="row">50% consensus</th>', topology)
        self.assertIn('aria-label="Download 50% consensus tree as PDF"', topology)
        self.assertIn("<figcaption>Projection of RF distances", mixing)
        self.assertIn("<figcaption>Split posterior probabilities across chains", mixing)
        self.assertEqual(fixed_topology.count('<figure class="plot-panel">'), 1)
        self.assertNotIn("Consensus-tree support levels", fixed_topology)
        self.assertNotIn("50% consensus-tree split support", fixed_mixing)
        self.assertNotIn("Projection of RF distances", fixed_mixing)
        self.assertNotIn("Split posterior probabilities across chains", fixed_mixing)
        self.assertNotIn("minimum topological", fixed_mixing)
        self.assertNotIn("ASDSF", fixed_mixing)
        self.assertNotIn("partitions.bs", fixed_topology)
        self.assertNotIn("<h4", topology + mixing)

    # A fixed topology has no topology-mixing work to perform; command-generation tests do not
    # otherwise exercise this early exit. Remove this if those diagnostics gain fixed-tree meaning.
    def test_skips_fixed_topology_diagnostic_work(self):
        analysis = Analysis.__new__(Analysis)
        analysis.sampled_topology_count = 1
        analysis.R_exe = True
        analysis.exec_show = lambda *args, **kwargs: self.fail("unexpected topology command")
        analysis.run_gnuplot = lambda *args, **kwargs: self.fail("unexpected topology plot")
        analysis.Rexec = lambda *args, **kwargs: self.fail("unexpected topology R command")

        analysis.compute_tree_mixing_diagnostics()
        analysis.compute_srq_plots()
        analysis.compute_tree_MDS()
        self.assertEqual(analysis.srq, [])

    # Each SRQ image must identify the sampled object rather than expose a stale template token;
    # command-generation coverage does not inspect plot titles. Remove this if SRQ plotting moves.
    def test_names_each_srq_plot(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            trees = directory / "chain.trees"
            trees.write_text("(a,b);\n", encoding="utf-8")
            for name in ("partitions.SRQ", "c50.SRQ"):
                output = directory / name
                output.write_text("0 0\n", encoding="utf-8")
                output_time = trees.stat().st_mtime + 10
                os.utime(output, (output_time, output_time))

            scripts = []
            analysis = Analysis.__new__(Analysis)
            analysis.outdir = directory
            analysis.sampled_topology_count = 2
            analysis.subpartitions = False
            analysis.subsample = 1
            analysis.burnin = 0
            analysis.until = None
            analysis.get_trees_files = lambda: [trees]
            analysis.run_gnuplot = scripts.append
            analysis.exec_show = lambda *args, **kwargs: self.fail("unexpected SRQ command")

            with redirect_stdout(io.StringIO()):
                analysis.compute_srq_plots()

        plot_script = "\n".join(scripts)
        self.assertIn("plot: partitions", plot_script)
        self.assertIn("plot: c50", plot_script)
        self.assertNotIn("$srq", plot_script)


class BPYSummarizeNavigationTests(unittest.TestCase):
    # Keep report navigation semantic and prevent links to sections omitted from reduced reports.
    # This can be removed if a different report renderer owns navigation and section selection.
    def test_builds_semantic_conditional_navigation(self):
        analysis = Analysis.__new__(Analysis)
        analysis.has_model = lambda: True
        analysis.has_parameters = lambda: True
        analysis.has_positive_selection = lambda: True
        analysis.has_trees = lambda: True
        analysis.has_alignments = lambda: True
        analysis.has_code = lambda: True
        analysis.sampled_topology_count = 2

        navigation = analysis.topbar()
        self.assertIn('<nav id="topbar" aria-label="Report sections">', navigation)
        self.assertIn('<a href="#data">Overview</a>', navigation)
        self.assertIn('<a href="#topology">Trees</a>', navigation)
        self.assertIn('<a href="#positive-selection">Positive selection</a>', navigation)
        self.assertIn('<a href="#mixing">Convergence &amp; mixing</a>', navigation)
        self.assertIn('<a href="#analysis">Run details</a>', navigation)
        self.assertIn('<a href="#models">Model &amp; priors</a>', navigation)
        self.assertIn("</nav>", navigation)
        self.assertNotIn("[<a", navigation)

        analysis.has_model = lambda: False
        analysis.has_parameters = lambda: False
        analysis.has_positive_selection = lambda: False
        analysis.has_trees = lambda: False
        analysis.has_alignments = lambda: False
        analysis.has_code = lambda: False

        navigation = analysis.topbar()
        self.assertNotIn('href="#data"', navigation)
        self.assertNotIn('href="#parameters"', navigation)
        self.assertNotIn('href="#positive-selection"', navigation)
        self.assertNotIn('href="#topology"', navigation)
        self.assertNotIn('href="#alignment"', navigation)
        self.assertNotIn('href="#models"', navigation)
        self.assertNotIn('href="#code"', navigation)
        self.assertNotIn('href="#mixing"', navigation)
        self.assertIn('href="#analysis"', navigation)

        analysis.has_trees = lambda: True
        analysis.sampled_topology_count = 1
        navigation = analysis.topbar()
        self.assertIn('href="#topology"', navigation)
        self.assertNotIn('href="#mixing"', navigation)

        header = analysis.html_header("report")
        self.assertIn('<html lang="en">', header)
        self.assertIn('<meta charset="utf-8">', header)
        self.assertIn('<meta name="viewport" content="width=device-width, initial-scale=1">', header)
        self.assertIn('<a class="skip-link" href="#report">Skip to report content</a>', header)
        self.assertIn("position: sticky;", header)
        self.assertIn("overflow-x: auto;", header)
        self.assertIn("#topbar #menu a:focus-visible", header)
        self.assertNotIn("position: fixed;", header)
        self.assertNotIn(":target:before", header)
        self.assertNotIn("//", header)
        self.assertIn("@media print", header)
        self.assertIn("#topbar, .skip-link {display:none;}", header)
        self.assertIn(".phylogeny-grid {grid-template-columns:repeat(3, minmax(0, 1fr));}", header)
        self.assertIn(".mixing-overview, .tree-mixing-grid", header)
        self.assertIn(".table-scroll {overflow:visible;}", header)
        self.assertIn("figure, tr, img, object {break-inside:avoid;}", header)
        self.assertIn("thead {display:table-header-group;}", header)
        self.assertIn("</main>", analysis.section_end())


if __name__ == "__main__":
    unittest.main()
