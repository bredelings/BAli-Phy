import json
from pathlib import Path
import sys


# Selection and ordering must remain independent; the increasing order must not
# change the highest selected representative chosen for each column.
report = json.loads((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
assert report["selection"] == {"kind": "above", "threshold": 1}
assert report["statistic"] == "median"
assert report["sort"] == "increasing"
assert [
    (row["column_index"], row["sequence"], row["statistics"]["mean"])
    for row in report["rows"]
] == [
    (4, "C", 2),
    (1, "C", 6),
    (3, "A", 8),
]
