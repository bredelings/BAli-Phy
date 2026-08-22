from pathlib import Path
import sys


# The conditioned report must present its probability, companion dN/dS, and source
# letter in a concise aligned table while retaining the conditioning sample counts.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
assert "Conditioned on positiveSelectionInModel = true (60 of 100 samples)" in report
table = report.split("\n\n", 1)[1].splitlines()
assert table == [
    "                                Posterior dN/dS",
    "Column  Codon  AA  Pr(dN/dS>1)        mean ± SD  Source letter",
    "     2  TGG    W         0.970    9.700 ± 0.200  C:1",
    "     4  GCT    A         0.990    9.900 ± 0.200  A:2",
    "     5  TTT    F         0.600    6.000 ± 0.300  A:3",
]
