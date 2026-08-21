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
require_equal(viewer["version"], 2)
require_equal(viewer["sequences"], ["alpha", "beta"])
require(".alignment-viewer-toolbar" in html, "viewer toolbar CSS is missing")
require("BaliPhyAlignmentViewer" in html, "viewer JavaScript is missing")
require("Blue–gray–red" in html, "palette label is missing")
require("palette: 'blue-gray-red'" in html, "the property display must default to the blue-gray-red palette")
require("Original colors" in html, "original-color checkbox is missing")
require("originalColorsCheckbox.checked = true" in html, "the viewer must initially show original alignment colors")
require("Model-averaged:" in html, "shared posterior-view sample summary is missing")
require("mean ± SD" in html, "complete posterior tooltip summary is missing")
require("alignment-viewer-report-scroll" in html, "column report panel is missing")
require("alignment-viewer-report-sort" in html, "sortable report-header styling is missing")
require("Minimum probability" not in html, "obsolete positive-selection threshold control remains")
require("reportSortSelect" not in html, "obsolete report-order control remains")
require(".alignment-scroll" not in html, "the alignment must retain document-level horizontal scrolling")
require(
    "position: sticky" in html and "width: calc(100vw - 1rem)" in html,
    "viewer panels must remain fixed during horizontal scrolling",
)
require_equal(properties["retained_samples"], 4)
require_equal(properties["properties"]["rate"]["mean"]["alpha"], [0.25, 2.0, 8.0])
require(DANGEROUS_PROPERTY in properties["properties"], "property name is missing from viewer JSON")
require(DANGEROUS_PROPERTY not in html, "property name was embedded as unsafe HTML")
require_equal(properties["properties"][DANGEROUS_PROPERTY]["mean"]["alpha"][0], 12345.6789)

reports = viewer["character_property_reports"]
generic = reports["rate"]["generic"]
require_equal(generic["kind"], "property-columns")
require_equal(
    [
        (row["column_index"], row["letter_count"], row["posterior_means"]["minimum"],
         row["posterior_means"]["middle"], row["posterior_means"]["maximum"])
        for row in generic["rows"]
    ],
    [(0, 2, 0.25, 0.25, 0.5), (1, 1, 4.0, 4.0, 4.0),
     (2, 1, 2.0, 2.0, 2.0), (3, 2, 8.0, 8.0, 16.0)],
)
require_equal(generic["table"]["columns"][2]["label"], "Mean minimum")
positive = reports["posSelection"]["positive_selection"]
require_equal(
    [
        (
            row["column_index"],
            row["sequence"],
            row["statistics"]["mean"],
            row["companion"]["statistics"]["mean"],
        )
        for row in positive["rows"]
    ],
    [
        (1, "beta", 0.7, 1.4),
        (2, "alpha", 0.96, 3.2),
        (3, "beta", 0.95, 2.8),
    ],
)

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
