# Conditioning must select after burn-in and summarize monotone draws without diagnostics.
# Compare TSV and reordered JSON with changing structure; ordinary reports do not cover filtered timelines.
# Remove this check if conditioning is removed.
import sys
from pathlib import Path
text = (Path(sys.argv[1]) / 'output').read_text(encoding="utf-8")
assert 'Matching samples [1] = 2' in text
assert 'Matching samples [2] = 2' in text
assert 'Matching samples = 4' in text
assert 'E omega = 3' in text
assert 'omega ~ 3' in text
assert 'Ne =' not in text and 'PSRF' not in text and '[increasing]' not in text
