import json
from pathlib import Path
import sys


report = json.loads((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
assert report["format"] == "bali-phy-character-property-report"
assert report["version"] == 3
assert report["kind"] == "property-columns"
assert report["property"] == "rate"
assert report["sort"] == "column"
assert report["statistic"] == "mean"
assert report["selection"] == {"kind": "all"}
assert report["retained_samples"] == 100
assert report["total_retained_samples"] == 100
assert report["condition"] is None
assert report["condition_value"] is None
assert [
    (
        row["column_index"],
        row["letter_count"],
        row["posterior_means"]["minimum"],
        row["posterior_means"]["middle"],
        row["posterior_means"]["maximum"],
    )
    for row in report["rows"]
] == [
    (0, 2, 1, 1, 1),
    (1, 2, 5, 5, 6),
    (3, 2, 7, 7, 8),
    (4, 3, 2, 2, 2),
]
