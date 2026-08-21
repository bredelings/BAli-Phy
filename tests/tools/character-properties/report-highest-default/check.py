import json
from pathlib import Path
import sys


# The implicit top 1% must retain at least one letter, include its cutoff tie,
# and group tied selected letters that occupy the same alignment column.
report = json.loads((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
assert report["selection"] == {"kind": "highest", "fraction": 0.01}
assert report["candidate_letters"] == 6
assert report["selected_letters"] == 2
assert len(report["rows"]) == 1
assert report["rows"][0]["column_index"] == 0
assert report["rows"][0]["sequence"] == "A"
