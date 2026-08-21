import csv
from pathlib import Path
import sys


# A prefixed posSelection name must resolve the dN/dS property with the same prefix;
# this becomes obsolete when summaries carry explicit companion-property metadata.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert rows[0]["companion-property"] == "foreground-dNdS"
assert rows[0]["companion-mean"] == "3"
