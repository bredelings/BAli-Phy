import csv
from pathlib import Path
import sys


# Selection and ordering must remain independent; the increasing order must not
# change the highest selected representative chosen for each column.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
reader = csv.DictReader(report.splitlines(), delimiter="\t")
# Ordinary selected reports have eight populated fields, without obsolete companion columns.
# This belongs to the selected-report check; all-column reports have a different schema.
assert reader.fieldnames == ["column", "sequence", "sequence-character", "symbol", "translation", "mean", "sd", "median"]
rows = list(reader)
assert all(None not in row for row in rows)
assert [(row["column"], row["sequence"], row["mean"]) for row in rows] == [
    ("5", "C", "2"),
    ("2", "C", "6"),
    ("4", "A", "8"),
]
