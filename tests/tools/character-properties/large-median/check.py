import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from _check_summary import read_summary, require_equal

summary = read_summary(sys.argv[1])
require_equal(summary["retained_samples"], 301)
require_equal(summary["properties"]["rate"]["median"], {"A": [150.0, 7.0]})
