# joint-indels Unit Tests

These tests verify the `--details` output format for joint-indels.

## Running Tests

```bash
./run-tests.sh /path/to/joint-indels
```

Or from the build directory:
```bash
./tests/tools/joint-indels/run-tests.sh ./build/src/joint-indels
```

## Test Cases

### Example 1: Single G1 Indel (M M M G1 M M)
- seq1 has a gap at position 3 relative to ancestor
- Expected output: `Start1=3, Start2=3, End1=3, End2=4, G1, Length=1`

### Example 2: Multi-character G1 Indel (M M G1 G1 G1 M)
- seq1 has 3 gaps at positions 2-4 relative to ancestor
- Expected output: `Start1=2, Start2=2, End1=2, End2=5, G1, Length=3`

### Example 3: G2 Indel (M G2 G2 M)
- seq2 has an insertion of 2 characters relative to ancestor
- Expected output: `Start1=1, Start2=1, End1=3, End2=1, G2, Length=2`

## Test File Format

Each test directory contains:
- `test.fastas` - MCMC alignment sample (with ancestral sequences)
- `test.trees` - Newick tree with internal node labels
- `command.txt` - Arguments to pass to joint-indels
- `expected-output` - Expected stdout

## Output Format

The `--details` output columns:
- **Start1/Start2**: Characters in seq1/seq2 before the indel
- **End1/End2**: Characters in seq1/seq2 after the indel
- **Type**: G1 (gap in seq1) or G2 (gap in seq2)
- **Length**: Number of characters in the indel
- **Len1/Len2**: Total length of seq1/seq2
