import json
from pathlib import Path
import sys


report = json.loads((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
assert report["sort"] == "increasing"
assert report["selection"] == {"kind": "above", "threshold": 0.95}
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
    (3, "A", 0.97, "dNdS", 3),
]
