import csv
from pathlib import Path
import sys


# The implicit top 1% must retain at least one letter, include its cutoff tie,
# even when the tie spans distinct alignment columns.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert [(row["column"], row["sequence"], row["mean"]) for row in rows] == [
    ("1", "A", "10"),
    ("2", "B", "10"),
]
