from html.parser import HTMLParser
import json


# Fail a structural check even when Python assertions are disabled.
def require(condition, message):
    if not condition:
        raise AssertionError(message)


# Compare structured values and include both values in a failed check.
def require_equal(obtained, expected):
    if obtained != expected:
        raise AssertionError(f"expected {expected!r}, obtained {obtained!r}")


class AlignmentHTMLParser(HTMLParser):
    """Collect the centralized viewer JSON and coordinate-bearing cells."""

    # Initialize the state needed while walking the generated document.
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.viewer_scripts = []
        self.cells = []
        self._script_chunks = None
        self._cell = None

    # Start collecting only the viewer-data script and alignment cells.
    def handle_starttag(self, tag, attrs):
        attributes = dict(attrs)
        if tag == "script" and attributes.get("id") == "alignment-viewer-data":
            self._script_chunks = []
        if tag == "td" and "alignment-cell" in attributes.get("class", "").split():
            self._cell = {"attributes": attributes, "text": []}

    # Finish the current viewer script or alignment cell.
    def handle_endtag(self, tag):
        if tag == "script" and self._script_chunks is not None:
            self.viewer_scripts.append("".join(self._script_chunks))
            self._script_chunks = None
        if tag == "td" and self._cell is not None:
            self._cell["text"] = "".join(self._cell["text"]).strip()
            self.cells.append(self._cell)
            self._cell = None

    # Append text to whichever interesting element is currently open.
    def handle_data(self, data):
        if self._script_chunks is not None:
            self._script_chunks.append(data)
        if self._cell is not None:
            self._cell["text"].append(data)


# Parse the generated document and require exactly one centralized data object.
def parse_viewer_html(html):
    parser = AlignmentHTMLParser()
    parser.feed(html)
    if len(parser.viewer_scripts) != 1:
        raise AssertionError(f"expected one alignment-viewer-data script, got {len(parser.viewer_scripts)}")
    return parser, json.loads(parser.viewer_scripts[0])


# Return character properties from either supported viewer JSON layout.
def character_properties_from_viewer(viewer):
    if viewer.get("format") == "bali-phy-character-properties":
        return viewer
    for key in ("character_properties", "characterProperties"):
        value = viewer.get(key)
        if isinstance(value, dict):
            return value
    raise AssertionError("viewer JSON does not contain character properties")


# Convert parsed cells to compact tuples used by the coordinate checks.
def cell_coordinates(cells):
    coordinates = []
    for cell in cells:
        attributes = cell["attributes"]
        character = attributes.get("data-character", "-1")
        coordinates.append(
            (
                int(attributes["data-sequence"]),
                int(attributes["data-column"]),
                int(character),
                cell["text"],
            )
        )
    return coordinates
