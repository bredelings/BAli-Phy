# Conditioning must select after burn-in and summarize monotone draws without diagnostics.
# Ordinary statreport tests do not cover filtered timelines; remove this if conditioning is removed.
import sys
from pathlib import Path
text = (Path(sys.argv[1]) / 'output').read_text()
assert 'Matching samples [1] = 2' in text
assert 'Matching samples = 2' in text
assert 'E omega = 3' in text
assert 'omega ~ 3' in text
assert 'Ne =' not in text and 'PSRF' not in text and '[increasing]' not in text
