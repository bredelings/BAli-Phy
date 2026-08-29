from pathlib import Path
import sys


# Paired reports must count both posterior views and use one conditioned representative
# when their preferred source letters differ; this protects model-averaged site reporting.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
assert "2 columns have model-averaged Pr(dN/dS>1) above 0.5." in report
assert (
    "When conditioned on positiveSelectionInModel = true (60 of 100 retained samples), "
    "3 columns exceed 0.5."
) in report
table = report.split("\n\n", 1)[1].splitlines()
assert table == [
    "                         Model-averaged posterior                   Conditioned posterior",
    "Column  Codon  AA  Pr(dN/dS>1)  dN/dS mean ± SD  Pr(dN/dS>1)  dN/dS mean ± SD  Source letter",
    "     1  ATG    M         0.800    0.200 ± 0.100        0.200    2.000 ± 0.100  B:1",
    "     4  GCT    A         0.600    0.900 ± 0.100        0.990    9.900 ± 0.200  A:2",
    "     2  TGG    W         0.070    0.700 ± 0.100        0.970    9.700 ± 0.200  C:1",
    "     5  TTT    F         0.060    0.600 ± 0.100        0.600    6.000 ± 0.300  A:3",
]
