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
    # Preserve simple scalar assignments without treating multi-assignment annotations as statistics.
    # This can be removed if bp-summarize stops parsing statreport's human-readable output.
    def test_fallback_accepts_only_one_complete_assignment(self):
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            log_file = directory / "C1.log"
            log_file.write_text("iter\tparameter\n0\t0\n", encoding="utf-8")
            report_file = directory / "Report"
            report_file.write_text(
                """\
M3_test:LogOddsPosSelection:  posterior-probability = 0.5702  [log-odds = 0.2825]
M3_test:LogOddsPosSelection ~ 0.1548  (NA,NA) @ 95%
                              t @ 21.24   Ne = 4   burnin = Not Converged!

constant = 3
""",
                encoding="utf-8",
            )
            report_time = log_file.stat().st_mtime + 10
            os.utime(report_file, (report_time, report_time))

            analysis = Analysis.__new__(Analysis)
            analysis.outdir = directory
            analysis.get_log_files = lambda: [log_file]
            analysis.subsample = None
            analysis.burnin = None
            analysis.until = None
            analysis.exec_show = lambda *args, **kwargs: self.fail("fresh Report should be reused")

            with redirect_stdout(io.StringIO()):
                analysis.summarize_numerical_parameters()

            self.assertEqual(analysis.median["M3_test:LogOddsPosSelection"], "0.1548")
            self.assertEqual(analysis.median["constant"], "3")
            self.assertNotIn("posterior-probability = 0.5702  [log-odds", analysis.median)


if __name__ == "__main__":
    unittest.main()
