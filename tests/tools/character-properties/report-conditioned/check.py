import json
from pathlib import Path
import sys


report = json.loads((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
# The conditioned report must select the primary property and its companion from
# one posterior view while retaining both matching and unconditional counts.
assert report["condition"] == "positiveSelectionInModel"
assert report["condition_value"] is True
assert report["retained_samples"] == 60
assert report["total_retained_samples"] == 100
assert [
    (
        row["column_index"],
        row["sequence"],
        row["statistics"]["mean"],
        row["companion"]["statistics"]["mean"],
    )
    for row in report["rows"]
] == [
    (3, "A", 0.99, 9.9),
    (1, "C", 0.97, 9.7),
    (4, "A", 0.6, 6),
]
