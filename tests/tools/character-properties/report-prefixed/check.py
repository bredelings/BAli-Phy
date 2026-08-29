import csv
from pathlib import Path
import sys


# A prefixed posSelection name must resolve the dN/dS property with the same prefix;
# this becomes obsolete when summaries carry explicit positive-selection and dN/dS roles.
report = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
rows = list(csv.DictReader(report.splitlines(), delimiter="\t"))
assert rows[0]["model-averaged-dNdS-mean"] == "3"
