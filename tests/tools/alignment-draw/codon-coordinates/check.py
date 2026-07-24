from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_support import cell_coordinates, parse_viewer_html, require_equal


html = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
parser, _ = parse_viewer_html(html)

require_equal(
    cell_coordinates(parser.cells),
    [
        (0, 0, 0, "AAA"),
        (0, 1, 1, "CCC"),
        (0, 2, -1, "---"),
        (0, 3, 2, "GGG"),
        (1, 0, 0, "AAA"),
        (1, 1, -1, "---"),
        (1, 2, 1, "TTT"),
        (1, 3, 2, "GGG"),
    ],
)
