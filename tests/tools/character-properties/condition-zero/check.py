import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1]))
from _check_summary import read_summary, require_equal


summary = read_summary(sys.argv[1])
# A declared but unsatisfied condition must remain distinguishable from a missing
# condition without attempting to calculate moments from an empty posterior sample.
require_equal(
    summary["conditioned"]["positiveSelectionInModel"],
    {
        "condition_value": True,
        "retained_samples": 0,
        "retained_samples_by_chain": [0],
        "properties": {},
    },
)
