from pathlib import Path
import sys


# The conditioned report must select the primary property and its companion from
# one posterior view while retaining both matching and unconditional counts.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
assert "Posterior view: positiveSelectionInModel = true" in report
assert "Matching samples: 60 of 100" in report
assert "2  C  1  TGG  W  0.97 +/- 0.02  0.98  dNdS  9.7 +/- 0.2  9.8" in report
assert "4  A  2  GCT  A  0.99 +/- 0.01  1  dNdS  9.9 +/- 0.2  10" in report
assert "5  A  3  TTT  F  0.6 +/- 0.1  0.6  dNdS  6 +/- 0.3  6" in report
