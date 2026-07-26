import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from _check_summary import read_summary, require_equal

summary = read_summary(sys.argv[1])
require_equal(summary["format"], "bali-phy-character-properties")
require_equal(summary["version"], 1)
require_equal(summary["coordinates"], {"kind": "ungapped-sequence-character", "index_base": 0})
require_equal(summary["selection"], {"skip": None, "until": None, "subsample": 1})
require_equal(summary["retained_samples"], 2)
require_equal(summary["retained_samples_by_chain"], [2])
require_equal(
    summary["properties"]["rate"],
    {
        "mean": {"A": [3.5, 15.0], "B": [2.5]},
        "sd": {"A": [2.5, 5.0], "B": [0.5]},
        "median": {"A": [1.0, 10.0], "B": [2.0]},
    },
)
require_equal(
    summary["properties"]["score"],
    {
        "mean": {"A": [2.0, 6.0], "B": [1.5]},
        "sd": {"A": [3.0, 2.0], "B": [0.5]},
        "median": {"A": [-1.0, 4.0], "B": [1.0]},
    },
)
