from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_support import parse_viewer_html, require_equal


html = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
_, viewer = parse_viewer_html(html)
uncertainty = viewer["alignment_uncertainty"]

require_equal(uncertainty["kind"], "posterior-alignment-probability")
require_equal(uncertainty["coordinates"], {"kind": "alignment-grid-cell", "index_base": 0})
require_equal(uncertainty["mean"], [[0.1, None, 0.7, 1.0], [0.2, 0.4, 0.8, 0.0]])
