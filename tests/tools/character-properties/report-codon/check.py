import json
from pathlib import Path
import sys


report = json.loads((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
assert [
    (row["column_index"], row["symbol"], row["translation"])
    for row in report["rows"]
] == [
    (0, "ATA", "M"),
    (1, "GCC", "A"),
    (2, "GCT", "A"),
]
