import json
from pathlib import Path
import sys


report = json.loads((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
assert report["minimum_probability"] == 0.5
assert len(report["rows"]) == 3
assert all(row["companion"] is None for row in report["rows"])
