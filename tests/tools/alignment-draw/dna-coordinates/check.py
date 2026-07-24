from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_support import (
    cell_coordinates,
    character_properties_from_viewer,
    parse_viewer_html,
    require,
    require_equal,
)


DANGEROUS_PROPERTY = "closing</script><script>alert(1)</script>"
html = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
parser, viewer = parse_viewer_html(html)
properties = character_properties_from_viewer(viewer)

require_equal(viewer["format"], "bali-phy-alignment-viewer")
require_equal(viewer["version"], 1)
require_equal(viewer["sequences"], ["alpha", "beta"])
require(".alignment-viewer-toolbar" in html, "viewer toolbar CSS is missing")
require("BaliPhyAlignmentViewer" in html, "viewer JavaScript is missing")
require("Blue–gray–red" in html, "palette label is missing")
require_equal(properties["retained_samples"], 4)
require_equal(properties["properties"]["rate"]["mean"]["alpha"], [0.25, 2.0, 8.0])
require(DANGEROUS_PROPERTY in properties["properties"], "property name is missing from viewer JSON")
require(DANGEROUS_PROPERTY not in html, "property name was embedded as unsafe HTML")
require_equal(properties["properties"][DANGEROUS_PROPERTY]["mean"]["alpha"][0], 12345.6789)

for cell in parser.cells:
    for attribute in cell["attributes"]:
        require(
            attribute not in {"data-property", "data-value", "data-mean", "data-count"},
            f"cell retains obsolete attribute {attribute!r}",
        )

require_equal(
    cell_coordinates(parser.cells),
    [
        (0, 0, 0, "A"),
        (0, 1, -1, "-"),
        (0, 2, 1, "C"),
        (0, 3, 2, "G"),
        (1, 0, 0, "A"),
        (1, 1, 1, "T"),
        (1, 2, -1, "-"),
        (1, 3, 2, "G"),
    ],
)
