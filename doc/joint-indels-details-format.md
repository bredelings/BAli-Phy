# Joint-Indels --details Output Format

This document explains the position format for the `joint-indels --details` option.

## Understanding G1 and G2

In BAli-Phy's pairwise HMM:
- **M** = match (both sequences emit a character)
- **G1** = gap in sequence 1 (seq2 emits a character, seq1 has a gap)
- **G2** = gap in sequence 2 (seq1 emits a character, seq2 has a gap)

## The Position Format

Report **character counts** in each sequence before and after the indel:
- **Start1, Start2**: Characters emitted in seq1/seq2 *before* the indel
- **End1, End2**: Characters emitted in seq1/seq2 *after* the indel

The difference `End - Start` encodes the insertion length in that sequence.

## Output Columns

| Column | Description |
|--------|-------------|
| Sample | Sample number (0-indexed) |
| Start1 | Characters in seq1 before this indel |
| Start2 | Characters in seq2 before this indel |
| End1   | Characters in seq1 after this indel |
| End2   | Characters in seq2 after this indel |
| Type   | I (insertion in seq2) or D (insertion in seq1) |
| Length | Number of characters in the indel |
| Len1   | Total length of seq1 |
| Len2   | Total length of seq2 |
| Sequence | The actual nucleotide/amino acid sequence of the indel |

For Type=I (internal state G1, gap in seq1): sequence is extracted from seq2.
For Type=D (internal state G2, gap in seq2): sequence is extracted from seq1.

**Note**: These sequences are reconstructed ancestral sequences at internal tree nodes, not observed leaf sequences. They vary across MCMC samples and depend on the substitution model and tree topology.

## Example 1: `M M M G1 M M` (single G1 insertion)

Tracing character counts:

| State | seq1 chars | seq2 chars |
|-------|------------|------------|
| M     | 1          | 1          |
| M     | 2          | 2          |
| M     | 3          | 3          |
| **G1**| 3          | 4          |
| M     | 4          | 5          |
| M     | 5          | 6          |

**The G1** (single insertion in seq2):
```
Start1=3, Start2=3, End1=3, End2=4, Type=I, Length=1, Len1=5, Len2=6, Sequence=X
```
(where X is the character at that position in seq2)
- Before: 3 chars in each sequence
- After: seq1 unchanged at 3, seq2 now has 4
- Length = End2 - Start2 = 1

## Example 2: `M M G1 G1 G1 M` (3-character G1 insertion)

| State | seq1 chars | seq2 chars |
|-------|------------|------------|
| M     | 1          | 1          |
| M     | 2          | 2          |
| **G1**| 2          | 3          |
| **G1**| 2          | 4          |
| **G1**| 2          | 5          |
| M     | 3          | 6          |

**The G1 block**:
```
Start1=2, Start2=2, End1=2, End2=5, Type=I, Length=3, Len1=3, Len2=6, Sequence=XYZ
```
(where XYZ are the 3 characters at those positions in seq2)
- seq2 gained 3 characters (5-2=3)
- seq1 unchanged

## Example 3: `M G2 G2 M` (2-character G2 insertion)

| State | seq1 chars | seq2 chars |
|-------|------------|------------|
| M     | 1          | 1          |
| **G2**| 2          | 1          |
| **G2**| 3          | 1          |
| M     | 4          | 2          |

**The G2 block**:
```
Start1=1, Start2=1, End1=3, End2=1, Type=D, Length=2, Len1=4, Len2=2, Sequence=XY
```
(where XY are the 2 characters at those positions in seq1)
- seq1 gained 2 characters (3-1=2)
- seq2 unchanged

## Example 4: `M M M G1 G2 G1 M M` (mixed indels)

This produces three separate indel events:

| State | seq1 chars | seq2 chars |
|-------|------------|------------|
| M     | 1          | 1          |
| M     | 2          | 2          |
| M     | 3          | 3          |
| **G1**| 3          | 4          |
| **G2**| 4          | 4          |
| **G1**| 4          | 5          |
| M     | 5          | 6          |
| M     | 6          | 7          |

Three indel events:
1. `Start1=3, Start2=3, End1=3, End2=4, Type=I, Length=1, Sequence=A` (from seq2)
2. `Start1=3, Start2=4, End1=4, End2=4, Type=D, Length=1, Sequence=B` (from seq1)
3. `Start1=4, Start2=4, End1=4, End2=5, Type=I, Length=1, Sequence=C` (from seq2)

## Why This Format

1. **No ambiguity**: Start ≠ End for the sequence with the insertion
2. **Length encoded**: `End - Start` directly gives insertion length
3. **Clear semantics**: The sequence with End > Start is the one with the insertion
4. **Proper ordering**: Indels can be unambiguously ordered by their Start values
