import csv
from pathlib import Path
import sys


# A strict probability threshold must retain the matching letter and its dN/dS summary;
# the conditioned-report test does not exercise an explicit threshold.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert list(rows[0]) == [
    "column", "sequence", "sequence-character", "symbol", "translation", "probability",
    "dNdS-mean", "dNdS-sd", "dNdS-median",
]
assert [(row["column"], row["probability"], row["dNdS-mean"]) for row in rows] == [("4", "0.97", "3")]
