# alignment-draw character-property tests

These focused tests cover the character-property input and embedded-HTML
contract.

List or run individual cases against an out-of-source build with:

```sh
python3 tests/tools/run-tests.py list tests/tools/alignment-draw
python3 tests/tools/run-tests.py run \
  tests/tools/alignment-draw/dna-coordinates \
  ../build/gcc-16-debug-O/src/alignment-draw
```

Meson registers each directory as a separate test. The structural checks parse
the generated HTML without a browser; the separate JavaScript test covers
scales, palettes, clipping, and AU color composition.
