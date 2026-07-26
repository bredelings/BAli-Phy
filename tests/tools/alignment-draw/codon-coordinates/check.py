from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_support import cell_coordinates, parse_viewer_html, require, require_equal


html = (Path(sys.argv[1]) / "output").read_text(encoding="utf-8")
parser, _ = parse_viewer_html(html)

require_equal(
    cell_coordinates(parser.cells),
    [
        (0, 0, 0, "ATA"),
        (0, 1, 1, "CCG"),
        (0, 2, -1, "---"),
        (0, 3, 2, "GTA"),
        (1, 0, 0, "ATA"),
        (1, 1, -1, "---"),
        (1, 2, 1, "TTC"),
        (1, 3, 2, "GTA"),
    ],
)
require(all(len(cell["parts"]) == 3 for cell in parser.cells), "codon cells must contain three nucleotide spans")
require(
    all("alignment-compound-cell" in cell["attributes"]["class"].split() for cell in parser.cells),
    "codon cells must let their colored spans fill the cell",
)
require_equal(parser.cells[0]["attributes"].get("data-amino-acid"), "M")
require("'Amino acid'" in html, "codon tooltips must identify the translated amino acid")
require(
    "appendTooltipRow(list, 'Display scale'" not in html,
    "tooltips must not present display scale as a character property",
)
require(
    "appendTooltipRow(list, 'Palette'" not in html,
    "tooltips must not present palette as a character property",
)
require("line-height: 1" in html, "alignment cells and their colored spans must use the same line height")
require(len({part["style"] for part in parser.cells[0]["parts"]}) == 2, "ATA must retain distinct A and T colors")
