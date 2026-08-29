import csv
from pathlib import Path
import sys


# Positive-selection reports must retain their fixed schema without the optional dN/dS property;
# this becomes obsolete if dN/dS is made mandatory at the data-model boundary.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert len(rows) == 3
assert all(
    row["model-averaged-dNdS-mean"] == row["model-averaged-dNdS-sd"]
    == row["model-averaged-dNdS-median"] == ""
    for row in rows
)
