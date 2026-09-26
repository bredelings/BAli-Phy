from pathlib import Path
import sys

# No pair has two exact calls: distinguish unavailable mismatch fractions from zero.
# This case complements the positive-denominator pair-enumeration check.
text = (Path(sys.argv[1]) / "output").read_text()
assert text.count("mean mismatch fraction = NA") == 2
