import itertools
from pathlib import Path
import re
import sys

# Compare the count-based statistic with explicit pairs, protecting gap and missing-data
# denominators on unequal overlaps. Site-parsimony tests do not exercise this report.
text = (Path(sys.argv[1]) / "output").read_text()
observed = re.findall(r"mean mismatch fraction = (.*)", text)
assert len(observed) == 2 and "minimum sequence identity" not in text
sequences = Path("input").read_text().splitlines()[1::2]
for include_gaps, value in zip((False, True), observed):
    differences = comparisons = 0
    for x, y in itertools.combinations(sequences, 2):
        for a, b in zip(x, y):
            exact = a in "ACGT" and b in "ACGT"
            gap = (a in "ACGT" and b == "-") or (b in "ACGT" and a == "-")
            if exact or (include_gaps and gap):
                comparisons += 1
                differences += a != b
    assert value == format(differences / comparisons, ".3g"), (value, differences, comparisons)
