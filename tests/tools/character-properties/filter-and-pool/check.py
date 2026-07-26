import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from _check_summary import read_summary, require_equal

summary = read_summary(sys.argv[1])
require_equal(summary["selection"], {"skip": 0, "until": 30, "subsample": 2})
require_equal(summary["retained_samples"], 4)
require_equal(summary["retained_samples_by_chain"], [2, 2])
require_equal(summary["properties"]["rate"]["mean"], {"A": [70.0]})
require_equal(summary["properties"]["rate"]["sd"], {"A": [50.99019513592785]})
require_equal(summary["properties"]["rate"]["median"], {"A": [30.0]})
