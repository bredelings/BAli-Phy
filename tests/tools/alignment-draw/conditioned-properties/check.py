from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_support import parse_viewer_html, require, require_equal


html = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
_, viewer = parse_viewer_html(html)
# The payload must keep each conditioned property table with reports generated from
# the same view; JavaScript only selects these precomputed, internally consistent sets.
condition = viewer["character_properties"]["conditioned"]["positiveSelectionInModel"]
require_equal(condition["retained_samples"], 60)
reports = viewer["conditioned_character_property_reports"]["positiveSelectionInModel"]
positive = reports["posSelection"]["positive_selection"]
require_equal(positive["condition"], "positiveSelectionInModel")
require_equal(positive["retained_samples"], 60)
require_equal(positive["total_retained_samples"], 100)
require_equal([row["column_index"] for row in positive["rows"]], [1, 3, 4])
require_equal(positive["rows"][1]["statistics"]["mean"], 0.99)
require_equal(positive["rows"][1]["companion"]["statistics"]["mean"], 9.9)
require("Posterior view" in html, "posterior-view selector code is missing")
