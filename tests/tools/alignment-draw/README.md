# alignment-draw character-property tests

These focused tests cover the character-property input and embedded-HTML
contract.

Run them against an out-of-source build with:

```sh
python3 tests/tools/alignment-draw/run-tests.py \
  ../build/gcc-16-debug-O/src/alignment-draw
```

The tests intentionally do not depend on a browser. They parse the generated
HTML to check grid-to-character coordinates, centralized property and AU data,
safe JSON embedding, self-contained viewer assets, codon-width handling, and
input validation. Browser-independent scale tests exercise automatic linear or
zero-aware logarithmic defaults, rank scaling, clipping, sequential and
median-diverging palettes, degenerate halves, and AU color composition.
