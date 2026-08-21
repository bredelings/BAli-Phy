import csv
from pathlib import Path
import sys


# Selection and ordering must remain independent; the increasing order must not
# change the highest selected representative chosen for each column.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert [(row["column"], row["sequence"], row["mean"]) for row in rows] == [
    ("5", "C", "2"),
    ("2", "C", "6"),
    ("4", "A", "8"),
]
