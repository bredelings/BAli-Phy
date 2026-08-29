import csv
from pathlib import Path
import sys


# A recorded condition with no true samples must remain a valid empty report for M1a-like models;
# this test becomes unnecessary if zero-sample conditioned summaries acquire a different representation.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert rows == []
assert report.startswith("column\tsequence\tsequence-character\tsymbol\ttranslation\tmodel-averaged-probability\t")
