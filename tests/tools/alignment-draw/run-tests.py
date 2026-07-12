#!/usr/bin/env python3

import argparse
from html.parser import HTMLParser
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


TEST_DIR = Path(__file__).resolve().parent
DANGEROUS_PROPERTY = "closing</script><script>alert(1)</script>"


class AlignmentHTMLParser(HTMLParser):
    """Collect the centralized viewer JSON and coordinate-bearing cells."""

    # Initialize the small amount of state needed while walking the document.
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
        raise AssertionError(
            f"expected one alignment-viewer-data script, got {len(parser.viewer_scripts)}"
        )
    return parser, json.loads(parser.viewer_scripts[0])


# Accept either a bare scientific result or a renderer wrapper around that result.
def character_properties_from_viewer(viewer):
    if viewer.get("format") == "bali-phy-character-properties":
        return viewer
    for key in ("character_properties", "characterProperties"):
        value = viewer.get(key)
        if isinstance(value, dict):
            return value
    raise AssertionError("viewer JSON does not contain character properties")


# Convert parsed cells to compact tuples used by the coordinate assertions.
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


class AlignmentDrawPropertyTests(unittest.TestCase):
    binary = None
    wrapper = []

    # Run alignment-draw with the common fixture arguments and capture all output.
    def run_draw(self, alignment, properties, alphabet, *extra):
        command = self.wrapper + [
            str(self.binary),
            str(TEST_DIR / alignment),
            "--properties",
            str(properties),
        ]
        if alphabet is not None:
            command.extend(["--alphabet", alphabet])
        command.extend(extra)
        return subprocess.run(command, text=True, capture_output=True, check=False)

    # Write a modified property result to a temporary file and invoke the tool.
    def run_with_property_data(self, data, alphabet="DNA"):
        with tempfile.TemporaryDirectory() as directory:
            filename = Path(directory) / "properties.json"
            filename.write_text(json.dumps(data), encoding="utf-8")
            return self.run_draw("dna.fasta", filename, alphabet)

    # Load a checked-in property fixture as mutable JSON for validation tests.
    def load_dna_properties(self):
        with (TEST_DIR / "dna-properties.json").open(encoding="utf-8") as stream:
            return json.load(stream)

    # Verify single-width coordinates, name-keyed values, and centralized safe JSON.
    def test_dna_coordinates_and_centralized_json(self):
        result = self.run_draw(
            "dna.fasta", TEST_DIR / "dna-properties.json", "DNA"
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        parser, viewer = parse_viewer_html(result.stdout)
        properties = character_properties_from_viewer(viewer)

        self.assertEqual(viewer["format"], "bali-phy-alignment-viewer")
        self.assertEqual(viewer["version"], 1)
        self.assertEqual(viewer["sequences"], ["alpha", "beta"])
        self.assertIn(".alignment-viewer-toolbar", result.stdout)
        self.assertIn("BaliPhyAlignmentViewer", result.stdout)
        self.assertIn("Blue–gray–red", result.stdout)
        self.assertEqual(properties["retained_samples"], 4)
        self.assertEqual(
            properties["properties"]["rate"]["mean"]["alpha"],
            [0.25, 2.0, 8.0],
        )
        self.assertIn(DANGEROUS_PROPERTY, properties["properties"])
        self.assertNotIn(DANGEROUS_PROPERTY, result.stdout)
        self.assertEqual(
            properties["properties"][DANGEROUS_PROPERTY]["mean"]["alpha"][0],
            12345.6789,
        )
        for cell in parser.cells:
            for attribute in cell["attributes"]:
                self.assertNotIn(attribute, {"data-property", "data-value", "data-mean", "data-count"})

        self.assertEqual(
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

    # Verify AU remains a separate sequence-major alignment-grid data layer.
    def test_alignment_uncertainty_uses_grid_coordinates(self):
        result = self.run_draw(
            "dna.fasta",
            TEST_DIR / "dna-properties.json",
            "DNA",
            "--AU",
            str(TEST_DIR / "dna-AU.prob"),
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        _, viewer = parse_viewer_html(result.stdout)
        uncertainty = viewer["alignment_uncertainty"]
        self.assertEqual(uncertainty["kind"], "posterior-alignment-probability")
        self.assertEqual(
            uncertainty["coordinates"],
            {"kind": "alignment-grid-cell", "index_base": 0},
        )
        self.assertEqual(
            uncertainty["mean"],
            [[0.1, None, 0.7, 1.0], [0.2, 0.4, 0.8, 0.0]],
        )

    # Verify an existing multi-character alphabet controls cells and coordinates.
    def test_codon_coordinates_use_alphabet_width(self):
        result = self.run_draw(
            "codons.fasta",
            TEST_DIR / "codon-properties.json",
            "Codons(DNA)",
            "--color-scheme",
            "DNA+contrast",
            "--AU",
            str(TEST_DIR / "codon-AU.prob"),
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        parser, _ = parse_viewer_html(result.stdout)
        self.assertEqual(
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

    # Reject AU data at a different grid resolution instead of truncating it.
    def test_rejects_trailing_alignment_uncertainty_rows(self):
        with tempfile.TemporaryDirectory() as directory:
            filename = Path(directory) / "too-long-AU.prob"
            source = (TEST_DIR / "dna-AU.prob").read_text(encoding="utf-8")
            filename.write_text(source + "0.5 0.5 0.5\n", encoding="utf-8")
            result = self.run_draw(
                "dna.fasta",
                TEST_DIR / "dna-properties.json",
                "DNA",
                "--AU",
                str(filename),
            )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("more than", result.stderr)

    # Verify another full alphabet name is resolved by the normal alphabet parser.
    def test_amino_acid_full_alphabet_name(self):
        result = self.run_draw(
            "dna.fasta", TEST_DIR / "dna-properties.json", "Amino-Acids"
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        parser, _ = parse_viewer_html(result.stdout)
        self.assertEqual(len(parser.cells), 8)

    # Reject property mode when token width cannot be established explicitly.
    def test_properties_require_alphabet(self):
        result = self.run_draw(
            "dna.fasta", TEST_DIR / "dna-properties.json", None
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("alphabet", result.stderr.lower())

    # Reject property files whose coordinate contract is not the supported one.
    def test_rejects_wrong_coordinate_contract(self):
        data = self.load_dna_properties()
        data["coordinates"]["index_base"] = 1
        result = self.run_with_property_data(data)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("index", result.stderr.lower())

    # Reject a property that omits a sequence present in the alignment.
    def test_rejects_missing_sequence(self):
        data = self.load_dna_properties()
        del data["properties"]["rate"]["mean"]["beta"]
        del data["properties"]["rate"]["count"]["beta"]
        result = self.run_with_property_data(data)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("beta", result.stderr)

    # Accept a fully validated property superset when the displayed view is pruned.
    def test_accepts_extra_property_sequences(self):
        data = self.load_dna_properties()
        for property_data in data["properties"].values():
            property_data["mean"]["pruned"] = [1.0, 2.0]
            property_data["count"]["pruned"] = [4, 4]
        result = self.run_with_property_data(data)
        self.assertEqual(result.returncode, 0, result.stderr)

    # Reject mean vectors that do not match the ungapped sequence length.
    def test_rejects_wrong_character_count(self):
        data = self.load_dna_properties()
        data["properties"]["rate"]["mean"]["alpha"] = [0.25, 2.0]
        data["properties"]["rate"]["count"]["alpha"] = [4, 4]
        result = self.run_with_property_data(data)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("alpha", result.stderr)
        self.assertIn("3", result.stderr)

    # Reject count arrays whose shape differs from the corresponding means.
    def test_rejects_mismatched_count_shape(self):
        data = self.load_dna_properties()
        data["properties"]["rate"]["count"]["alpha"] = [4, 4]
        result = self.run_with_property_data(data)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("count", result.stderr.lower())

    # Reject names that are not accepted by the existing alphabet factory.
    def test_rejects_unknown_alphabet(self):
        result = self.run_draw(
            "dna.fasta",
            TEST_DIR / "dna-properties.json",
            "Definitely-Not-An-Alphabet",
        )
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("alphabet", result.stderr.lower())


# Run the suite and translate expected failures into an explicit XFAIL result.
def main():
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument("--wrapper", action="append", default=[])
    argument_parser.add_argument("binary", type=Path)
    arguments = argument_parser.parse_args()

    AlignmentDrawPropertyTests.binary = arguments.binary.resolve()
    AlignmentDrawPropertyTests.wrapper = arguments.wrapper
    suite = unittest.defaultTestLoader.loadTestsFromTestCase(
        AlignmentDrawPropertyTests
    )
    result = unittest.TextTestRunner(verbosity=2).run(suite)

    expected_failure = (TEST_DIR / "xfail").exists()
    if expected_failure and not result.wasSuccessful():
        print("XFAIL: character-property viewer is not implemented")
        return 0
    if expected_failure and result.wasSuccessful():
        print("XPASS: remove tests/tools/alignment-draw/xfail", file=sys.stderr)
        return 1
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    sys.exit(main())
