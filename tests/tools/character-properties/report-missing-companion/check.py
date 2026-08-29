import csv
from pathlib import Path
import sys


# Positive-selection reports must retain their fixed schema without the optional dN/dS property;
# this becomes obsolete if dN/dS is made mandatory at the data-model boundary.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert len(rows) == 3
assert all(row["dNdS-mean"] == row["dNdS-sd"] == row["dNdS-median"] == "" for row in rows)
