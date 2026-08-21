import csv
from pathlib import Path
import sys


# A strict probability threshold must retain the matching letter and its dN/dS summary;
# the conditioned-report test does not exercise an explicit threshold.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert [(row["column"], row["mean"], row["companion-mean"]) for row in rows] == [("4", "0.97", "3")]
