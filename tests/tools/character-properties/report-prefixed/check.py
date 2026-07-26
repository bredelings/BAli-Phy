import json
from pathlib import Path
import sys


report = json.loads((Path(sys.argv[1]) / "output").read_text(encoding="utf-8"))
assert report["rows"][0]["companion"]["property"] == "foreground-dNdS"
assert report["rows"][0]["companion"]["statistics"]["mean"] == 3
