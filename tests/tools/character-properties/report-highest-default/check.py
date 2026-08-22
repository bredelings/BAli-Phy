import csv
from pathlib import Path
import sys


# The implicit top 1% must retain exactly one letter when its rounded count is zero.
# Which tied letter represents that result is deliberately left unspecified.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert len(rows) == 1
assert rows[0]["mean"] == "10"
