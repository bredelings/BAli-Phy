from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from check_support import AlignmentHTMLParser, require_equal


parser = AlignmentHTMLParser()
parser.feed((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
require_equal(parser.ruler_labels, ["1", "10"])
