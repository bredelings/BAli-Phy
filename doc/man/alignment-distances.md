% alignment-distances(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-distances** - Compute distances between alignments.

# SYNOPSIS

**alignment-distances** \[OPTIONS\] _analysis_ _alignment-file1_ [_alignment-file2_ ...]

# DESCRIPTION

Compute distances between alignments. Sample files contain FASTA alignments separated by empty
lines; `-` reads standard input. Internal-node placeholders are removed before matching sequences
by name. Names must be unique and ungapped lengths must agree; row order may differ.

Analysis modes and inputs:

- **score**: one reference alignment followed by one or more sample files; report each requested measure.
- **AxA**: one or more sample files; report a matrix over all retained alignments using one measure.
- **NxN**: one reference alignment and one sample file; report a matrix of sequence-pair scores,
  averaged over sampled alignments. Accepts `pairwise`, `nonrecall`, or `inaccuracy`.
- **compare**: two sample files; summarize within-group and between-group distances.
- **median**: one sample file; write the retained alignment with smallest average distance to the others.
- **distances**: one sample file; summarize pairwise distances and each alignment's average distance.

The last three modes require one symmetric distance: `splits`, `splits2`, or `pairwise`.
`score` and `AxA` also accept `recall`, `accuracy`, `nonrecall`, and `inaccuracy`; recall and accuracy
are similarities, while their complements are directional losses. Ratios with zero denominators
are undefined and reported as NaN. They are not replaced with zero or one.

`NxN` reports disagreement by default: identical alignments give zero. Its `pairwise` scores are
normalized by the two sequence lengths; the whole-alignment `pairwise` measure is an unnormalized
count. A singleton sample is a valid median, but has no pairwise summary. Median diagnostics use
zero-based ranks and report the mean pairwise distance, not the maximum distance.

# INPUT OPTIONS:
**-h**, **`--help`**
: Produce help message

**-s** _arg_ (=0), **`--skip`** _arg_ (=0)
: Number of alignments to skip per sample file; does not apply to the reference.

**-m** _arg_ (=1000), **`--max`** _arg_ (=1000)
: Maximum retained alignments per sample file after thinning; `-1` means unlimited.
  Does not apply to the reference, which must contain exactly one alignment.

**-V**, **`--verbose`**
: Output more log messages on stderr.

**`--alphabet`** _arg_
: Specify the alphabet: DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.


# ANALYSIS OPTIONS:
**`--distances`** _arg_
: Colon-separated measures for `score`; exactly one measure for other modes. Defaults to
  `splits:splits2:nonrecall:inaccuracy` for `score`, `pairwise` for `NxN`, and `splits` otherwise.

**`--analysis`** _arg_
: Analysis: score, AxA, NxN, compare, median, distances

**`--CI`** _arg_ (=0.95)
: Central interval probability for `compare` and `distances`. This describes the distance
  distribution, not uncertainty in its mean.

**`--mean`**
: Show mean and standard deviation in `compare` and `distances`.

**`--median`**
: Show median and central interval in `compare` and `distances` (the default report).

**`--minmax`**
: Show minimum and maximum distances in `compare` and `distances`.


# EXAMPLES:
 
Compute distances from true.fasta to each in As.fasta:
```
% alignment-distances score true.fasta As.fasta
```

Compute distance matrix between all pairs of alignments in all files:
```
% alignment-distances AxA file1.fasta ... fileN.fasta
```

Compute NxN sequence-pair disagreement scores, averaged over As:
```
% alignment-distances NxN true.fasta As.fasta
```

Find alignment with smallest average distance to other alignments:
```
% alignment-distances median As.fasta > A.fasta
```

Compare the distances within and between the two groups:
```
% alignment-distances compare A-dist1.fasta A-dist2.fasta
```

Report distribution of average distance to other alignments:
```
% alignment-distances distances As.fasta
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

