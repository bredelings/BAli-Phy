# joint-indels --details Test Cases

## Terminology

A branch splits the tree into two sets of taxa (a bipartition). The `--partition <taxon>` option identifies which branch by naming one taxon from one side.

For the pairwise alignment on that branch:
- **Sequence 1**: The side containing the named taxon
- **Sequence 2**: The other side

Indel types:
- **I (Insertion)**: Sequence 2 has characters where Sequence 1 has a gap (internal state G1)
- **D (Deletion)**: Sequence 1 has characters where Sequence 2 has a gap (internal state G2)

Output columns:
- `Start1/Start2`: Characters emitted to each sequence before the indel
- `End1/End2`: Characters emitted to each sequence at the end of the indel
- `Len1/Len2`: Total sequence lengths (for context; these vary across MCMC samples)
- `Sequence`: The actual nucleotide/amino acid sequence of the indel

The indel length is `End - Start` for the sequence that received characters.

For Type=I (insertion in Seq2): sequence is extracted from Seq2.
For Type=D (insertion in Seq1): sequence is extracted from Seq1.

## Tree Structure (all examples)

```
((seq1:0.1,seq2:0.1)A4:0.1,seq3:0.2)A5
```

---

## Example 1: Triple Insertion

**Command**: `--partition seq1`

**Alignment**:
```
seq1:  AC---A  (3 chars)  <- partition side (Sequence 1)
seq2:  ACGGGA  (6 chars)  <- other side (Sequence 2)
seq3:  ACGGGA
A4:    ACGGGA
A5:    ACGGGA
```

**Pairwise alignment states**: `M M G1 G1 G1 M`

| State | Seq1 gets | Seq2 gets | Cumulative Seq1 | Cumulative Seq2 |
|-------|-----------|-----------|-----------------|-----------------|
| M     | A         | A         | 1               | 1               |
| M     | C         | C         | 2               | 2               |
| G1    | (gap)     | G         | 2               | 3               |
| G1    | (gap)     | G         | 2               | 4               |
| G1    | (gap)     | G         | 2               | 5               |
| M     | A         | A         | 3               | 6               |

**Output**: `Start1=2, Start2=2, End1=2, End2=5, I, Length=3, Len1=3, Len2=6, Sequence=GGG`

- Before the G1 block: 2 characters emitted to each
- After the G1 block: Seq1 still at 2, Seq2 advances to 5
- Type = I (Insertion): Seq2 gained 3 characters (End2 - Start2 = 5 - 2 = 3)

---

## Example 2: Deletion

**Command**: `--partition seq2` (note: different partition!)

**Alignment**:
```
seq1:  A--A  (2 chars)  <- other side (Sequence 2)
seq2:  AGGA  (4 chars)  <- partition side (Sequence 1)
seq3:  A--A
A4:    A--A
A5:    A--A
```

**Pairwise alignment states**: `M G2 G2 M`

| State | Seq1 gets | Seq2 gets | Cumulative Seq1 | Cumulative Seq2 |
|-------|-----------|-----------|-----------------|-----------------|
| M     | A         | A         | 1               | 1               |
| G2    | G         | (gap)     | 2               | 1               |
| G2    | G         | (gap)     | 3               | 1               |
| M     | A         | A         | 4               | 2               |

**Output**: `Start1=1, Start2=1, End1=3, End2=1, D, Length=2, Len1=4, Len2=2, Sequence=GG`

- Before the G2 block: 1 character emitted to each
- After the G2 block: Seq1 advances to 3, Seq2 still at 1
- Type = D (Deletion): Seq2 has a gap where Seq1 has characters (End1 - Start1 = 3 - 1 = 2)

---

## Example 3: Mixed Indels

**Command**: `--partition seq1`

**Alignment**:
```
seq1:  ACG-T-AA  (6 chars)  <- partition side (Sequence 1)
seq2:  ACGC-TAA  (7 chars)  <- other side (Sequence 2)
seq3:  ACGC-TAA
A4:    ACGC-TAA
A5:    ACGC-TAA
```

**Pairwise alignment states**: `M M M G1 G2 G1 M M`

| State | Seq1 gets | Seq2 gets | Cumulative Seq1 | Cumulative Seq2 |
|-------|-----------|-----------|-----------------|-----------------|
| M     | A         | A         | 1               | 1               |
| M     | C         | C         | 2               | 2               |
| M     | G         | G         | 3               | 3               |
| G1    | (gap)     | C         | 3               | 4               |
| G2    | T         | (gap)     | 4               | 4               |
| G1    | (gap)     | T         | 4               | 5               |
| M     | A         | A         | 5               | 6               |
| M     | A         | A         | 6               | 7               |

**Output** (3 indels):
```
Start1=3, Start2=3, End1=3, End2=4, I, Length=1, Len1=6, Len2=7, Sequence=C
Start1=3, Start2=4, End1=4, End2=4, D, Length=1, Len1=6, Len2=7, Sequence=T
Start1=4, Start2=4, End1=4, End2=5, I, Length=1, Len1=6, Len2=7, Sequence=T
```

This example shows:
- First G1: Insertion of C into Seq2 (Seq1 has gap)
- G2: Deletion - T in Seq1 where Seq2 has gap
- Second G1: Insertion of T into Seq2 (Seq1 has gap)

---

## Summary

| Example | Partition | Pattern | Indel Types | Seq1 | Seq2 |
|---------|-----------|---------|-------------|------|------|
| 1       | seq1      | `M M G1 G1 G1 M` | I (×3 chars) | 3 chars | 6 chars |
| 2       | seq2      | `M G2 G2 M` | D (×2 chars) | 4 chars | 2 chars |
| 3       | seq1      | `M M M G1 G2 G1 M M` | I, D, I | 6 chars | 7 chars |

---

## Running Tests

```bash
./run-tests.sh /path/to/joint-indels
```

Or via meson:
```bash
meson test -C build "joint-indels --details"
```

## Test File Format

Each test directory contains:
- `test.fastas` - MCMC alignment sample (with ancestral sequences)
- `test.trees` - Newick tree with internal node labels
- `args.txt` - Arguments to pass to joint-indels
- `expected-output` - Expected stdout
