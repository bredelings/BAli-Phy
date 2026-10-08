% alignment-distances(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-distances** - Compute distances between alignments.

# SYNOPSIS

**alignment-distances** \[OPTIONS\] **score** _REFERENCE_ _SAMPLE_...

**alignment-distances** \[OPTIONS\] **AxA** _SAMPLE_...

**alignment-distances** \[OPTIONS\] **NxN** _REFERENCE_ _SAMPLE_

**alignment-distances** \[OPTIONS\] **compare** \[REPORT-OPTIONS\] _SAMPLE1_ _SAMPLE2_

**alignment-distances** \[OPTIONS\] **median** _SAMPLE_

**alignment-distances** \[OPTIONS\] **distances** \[REPORT-OPTIONS\] _SAMPLE_

# DESCRIPTION

Compute distances between alignments. Sample files contain FASTA alignments separated by empty
lines; `-` reads standard input. Internal-node placeholders are removed before matching sequences
by name. Names must be unique and ungapped lengths must agree; row order may differ.

Exactly one command is required. Reference and sample filenames are positional arguments.
Shared options may appear before or after the command. Reporting options apply only to
`compare` and `distances` and must follow the command. Use `alignment-distances COMMAND --help`
for command-specific help.

Commands and inputs:

- **score**: one reference alignment followed by one or more sample files; report each requested measure.
- **AxA**: one or more sample files; report a matrix over all retained alignments using one measure.
- **NxN**: one reference alignment and one sample file; report a matrix of sequence-pair scores,
  averaged over sampled alignments. Accepts `pairwise`, `nonrecall`, or `inaccuracy`.
- **compare**: two sample files; summarize within-group and between-group distances.
- **median**: one sample file; write the retained alignment with smallest average distance to the others.
- **distances**: one sample file; summarize pairwise distances and each alignment's average distance.

`median` accepts `splits`, `splits2`, `pairwise`, `nonrecall`, and `inaccuracy`.
`score`, `AxA`, `compare`, and `distances` also accept the similarities `recall` and `accuracy`.
Only `score` accepts multiple measures. Ratios with zero denominators remain undefined (NaN)
in score and matrix output; `median`, `compare`, and `distances` report an error if a required
pairwise value is undefined, rather than omitting it or replacing it with zero or one.

For directional measures, `D(i,j)` uses alignment `i` as the first argument and `j` as the second.
Recall divides shared homologies by the number in `i`; accuracy divides by the number in `j`.
Nonrecall and inaccuracy are their complements. An alignment's average is its outgoing row mean,
excluding self-comparison. `median` minimizes that mean; switching between nonrecall and
inaccuracy reverses the direction. Recall and accuracy cannot be minimized by `median`.

`distances` summarizes all ordered pairs for directional measures. `compare` reports both
cross-group distributions, `D12` (group 1 to group 2) and `D21` (group 2 to group 1).
`D1(2)` and `D2(1)` summarize each alignment's outgoing mean to the other group;
`D1(1)` and `D2(2)` are within-group row means excluding self-comparison.
Probability comparisons give half weight to ties. Larger recall/accuracy values mean greater
agreement, not greater disagreement. Symmetric measures retain one value per unordered pair
and a single cross-group report, preserving their existing quantiles.

`NxN` reports disagreement by default: identical alignments give zero. Its `pairwise` scores are
normalized by the two sequence lengths; the whole-alignment `pairwise` measure is an unnormalized
count. A singleton sample is a valid median, but has no pairwise summary. Median diagnostics use
zero-based ranks and report the mean pairwise distance, not the maximum distance.

# SHARED OPTIONS:
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


**`--distances`** _arg_
: Colon-separated measures for `score`; exactly one measure for other modes. Defaults to
  `splits:splits2:nonrecall:inaccuracy` for `score`, `pairwise` for `NxN`, and `splits` otherwise.

# REPORTING OPTIONS:

These options follow `compare` or `distances` and are unavailable for other commands.

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

Summarize directional nonrecall between two samples:
```
% alignment-distances compare --distances=nonrecall sample1.fastas sample2.fastas
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

