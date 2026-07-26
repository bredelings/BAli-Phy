import json
from pathlib import Path
import sys


report = json.loads((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
assert report["sort"] == "mean-ascending"
assert report["minimum_probability"] == 0.95
assert [
    (
        row["column_index"],
        row["sequence"],
        row["statistics"]["mean"],
        row["companion"]["property"],
        row["companion"]["statistics"]["mean"],
    )
    for row in report["rows"]
] == [
    (1, "C", 0.95, "dNdS", 2.5),
    (3, "A", 0.97, "dNdS", 3),
]
