import json
from pathlib import Path
import sys


report = json.loads((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
assert report["format"] == "bali-phy-character-property-report"
assert report["version"] == 1
assert report["kind"] == "property"
assert report["property"] == "rate"
assert report["sort"] == "column"
assert report["retained_samples"] == 100
assert [
    (row["column_index"], row["sequence"], row["character_index"], row["symbol"], row["statistics"]["mean"])
    for row in report["rows"]
] == [
    (0, "A", 0, "A", 1),
    (1, "C", 0, "T", 6),
    (3, "A", 1, "C", 8),
    (4, "A", 2, "G", 2),
]
assert all(row["companion"] is None for row in report["rows"])
