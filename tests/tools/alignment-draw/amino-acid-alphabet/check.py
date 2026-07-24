from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_support import parse_viewer_html, require_equal


html = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
parser, _ = parse_viewer_html(html)
require_equal(len(parser.cells), 8)
