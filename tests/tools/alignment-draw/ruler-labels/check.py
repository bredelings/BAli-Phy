from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_support import AlignmentHTMLParser, require, require_equal


html = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
parser = AlignmentHTMLParser()
parser.feed(html)
require_equal(parser.ruler_labels, ["1", "10"])
require("bottom: 0.35em" in html, "ruler labels must leave clearance above the alignment")
