#!/usr/bin/env python3

from contextlib import redirect_stdout
import io
import os
from pathlib import Path
import runpy
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts" / "bp-summarize"
MODULE = runpy.run_path(str(SCRIPT))
Analysis = MODULE["Analysis"]


class BPSummarizeScalarTests(unittest.TestCase):
    # Keep statreport's sampled and constant formats distinct and preserve aligned paired summaries.
    # This can be removed if bp-summarize stops parsing and presenting statreport's text output.
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

            with redirect_stdout(io.StringIO()):
                analysis.summarize_numerical_parameters()

            self.assertEqual(commands, [["statreport", "--mean", "--median", log_file]])
            self.assertEqual(analysis.mean["M3_test:LogOddsPosSelection"], "0.1782")
            self.assertEqual(analysis.stddev["M3_test:LogOddsPosSelection"], "0.5034")
            self.assertEqual(analysis.median["M3_test:LogOddsPosSelection"], "0.1548")
            self.assertEqual(analysis.median["constant"], "3")
            self.assertNotIn("posterior-probability = 0.5702  [log-odds", analysis.median)

            section = analysis.section_scalar_variables()
            self.assertIn('<table class="backlit2 scalar-variables">', section)
            self.assertIn("Mean &plusmn; SD", section)
            self.assertIn("Median (95% BCI)", section)
            self.assertIn('<th colspan="3">Mean &plusmn; SD</th>', section)
            self.assertIn('<th colspan="2">Median (95% BCI)</th>', section)
            self.assertIn('<td class="scalar-mean">0.1782</td>', section)
            self.assertIn('<td class="scalar-pm">&plusmn;</td>', section)
            self.assertIn('<td class="scalar-sd">0.5034</td>', section)
            self.assertIn('<td class="scalar-median">0.1548</td>', section)
            self.assertIn('<td class="scalar-bci">(NA, NA)</td>', section)
            self.assertIn(
                """\
  <td>constant</td>
  <td class="scalar-mean"></td>
  <td class="scalar-pm"></td>
  <td class="scalar-sd"></td>
  <td class="scalar-median">3</td>
  <td class="scalar-bci"></td>
""",
                section,
            )
            self.assertNotIn("mean-sd", section)
            self.assertNotIn("median-bci", section)
            for row in section.split("<tr ")[1:]:
                self.assertEqual(row.split("</tr>", 1)[0].count("<td"), 11)

            header = analysis.html_header("scalar report")
            self.assertIn("table.scalar-variables td:not(:first-child) {text-align:right;}", header)
            self.assertIn("td.scalar-pm {padding-left:0; padding-right:0; text-align:center;}", header)
            self.assertNotIn("display:grid", header)


if __name__ == "__main__":
    unittest.main()
