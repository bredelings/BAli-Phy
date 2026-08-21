import csv
from pathlib import Path
import sys


# Partial codon alphabets must infer RNA while preserving the requested genetic code;
# this becomes obsolete if report translation moves to an independently tested layer.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert [(row["column"], row["symbol"], row["translation"]) for row in rows] == [
    ("1", "AUA", "M"),
    ("2", "GCC", "A"),
    ("3", "GCU", "A"),
]
