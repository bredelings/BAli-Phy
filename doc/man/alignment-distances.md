% alignment-distances(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-distances** - Compare alignments of the same sequences.

# SYNOPSIS

**alignment-distances** \[OPTIONS\] **score** _REFERENCE_ _SAMPLE_...

**alignment-distances** \[OPTIONS\] **AxA** _SAMPLE_...

**alignment-distances** \[OPTIONS\] **NxN** _REFERENCE_ _SAMPLE_

**alignment-distances** \[OPTIONS\] **compare** \[REPORT-OPTIONS\] _SAMPLE1_ _SAMPLE2_

**alignment-distances** \[OPTIONS\] **median** _SAMPLE_

**alignment-distances** \[OPTIONS\] **distances** \[REPORT-OPTIONS\] _SAMPLE_

# DESCRIPTION

Compare alternative alignments of the same sequences, summarize an alignment sample, or choose
one representative alignment. Comparisons concern which residues are aligned together, rather
than similarity between their nucleotide or amino-acid values.

A sample file contains FASTA alignments separated by empty lines. A reference file contains
exactly one alignment. Use `-` for standard input. Sequence names must be unique and match
across alignments; each sequence must have the same ungapped length. Sequence order may differ.
BAli-Phy internal-node placeholders and empty columns are removed before comparison.

Exactly one command is required. Filenames are positional arguments. Shared options may appear
before or after the command; reporting options must follow `compare` or `distances`.
Use `alignment-distances COMMAND --help` for command-specific help.

# COMMANDS

**score** _REFERENCE_ _SAMPLE_...
: Compare each sampled alignment with the reference. Write a tab-separated table with one row
  per retained alignment: its sample filename followed by the requested measures. The reference
  is the first argument of each comparison. Default measures: `splits:splits2:nonrecall:inaccuracy`.

**AxA** _SAMPLE_...
: Write a tab-separated matrix of distances between all retained alignments. Rows and columns
  follow file order, then alignment order within each file. There are no row or column labels.
  Entry `(i,j)` compares alignment `i` with alignment `j`. Default measure: `splits`.

**NxN** _REFERENCE_ _SAMPLE_
: Write a matrix comparing the alignment of each pair of sequences with the reference, averaged
  over retained sample alignments. Here `N` is the number of sequences, not alignments. A header
  lists sequence names in the order used for both rows and columns; rows have no labels.
  Accepts `pairwise`, `nonrecall`, or `inaccuracy`. Default measure: `pairwise`.

**compare** _SAMPLE1_ _SAMPLE2_
: Summarize distances within each sample and between samples. See OUTPUT for the report labels.
  Default measure: `splits`.

**median** _SAMPLE_
: Write the sampled alignment with the smallest mean distance to the other retained alignments,
  using the candidate as the first argument of each comparison. This selects an existing
  alignment; it does not construct a consensus. Ties select the first candidate in sample order.
  Diagnostics go to standard error. Accepts `splits`, `splits2`, `pairwise`, `nonrecall`, or
  `inaccuracy`. Default measure: `splits`.

**distances** _SAMPLE_
: Summarize distances between retained alignments and each alignment's mean distance to the
  others. Default measure: `splits`.

# MEASURES

A *residue pair* means two residues from different sequences placed in the same column.
Residues are identified by their positions in their sequences. For a comparison `D(A,B)`,
let `H(A)` and `H(B)` be the numbers of residue pairs in the two alignments, and `S` the number
shared by both.

**recall**, **accuracy**
: Fractions of residue pairs shared: `recall = S/H(A)` and `accuracy = S/H(B)`.
  Higher values mean greater agreement. In `score`, these measure the fraction of reference
  pairs recovered and the fraction of sampled pairs supported by the reference, respectively.

**nonrecall**, **inaccuracy**
: `1 - recall` and `1 - accuracy`. Lower values mean greater agreement; zero means no missing
  or unsupported pairs, respectively. These measures depend on argument order:
  `nonrecall(A,B) = inaccuracy(B,A)`.

**splits**, **splits2**
: Measure how columns are broken apart. If the residues of a column in `A` occupy `k` columns
  in `B`, that column contributes `k-1` to `splits` or `k*(k-1)/2` to `splits2`. Sum over columns
  in both directions. Thus `splits2` gives more weight to columns broken into many pieces.

**pairwise**
: For each pair of sequences, count residues whose aligned partner (a residue or gap) differs
  between alignments, summing the counts from both alignments. This is an unnormalized count.

`splits`, `splits2`, and `pairwise` are symmetric: exchanging alignments gives the same value.
The four fractional measures can be asymmetric. Only `score` accepts multiple measures;
`median` excludes `recall` and `accuracy` because it minimizes distance.

For `NxN`, measures apply separately to each pair of sequences. Its `pairwise` score is the
fraction of residues whose aligned partner differs, normalized by the sum of the two sequence
lengths. Values range from zero (agreement) to one (complete disagreement).

A fraction with a zero denominator is undefined. `score`, `AxA`, and `NxN` can report `NaN`;
`median`, `compare`, and `distances` report an error if a required comparison is undefined.

# OUTPUT

`distances` treats its input as sample 1; `compare` numbers its inputs 1 and 2.
`D11` and `D22` describe distances within samples 1 and 2,
excluding self-comparisons. Symmetric measures count each pair once; asymmetric measures count
both directions. `D12` describes comparisons from sample 1 to sample 2. Asymmetric measures
also produce `D21`, with the arguments reversed.

`D1(1)` describes the distribution of individual alignments' mean distances to the others in
sample 1; `D2(2)` does the same for sample 2. `D1(2)` contains one mean per alignment in sample 1,
comparing it with all alignments in sample 2. `D2(1)` reverses the sample roles. In every mean,
the individual alignment is the first argument of the distance function.

By default, each distribution is summarized by its median and central 95% interval. `--mean`
reports its mean and standard deviation; `--minmax` reports its range. The interval describes
variation in distances, not uncertainty in an estimated mean. Report flags can be combined.

`compare` also prints comparisons such as `P(D12 > D11)`: the fraction of cross-distribution
value pairs for which the first value is larger, counting ties as one half. These are descriptive
comparisons, not significance-test p-values. For `recall` and `accuracy`, larger means greater
agreement. A sample containing only one alignment has no within-sample summary.

`median` writes FASTA to standard output. Its standard-error diagnostics list up to five
candidates by increasing mean distance (`E D`), with ranks starting at zero, followed by mean
distances among the best candidates and among all retained alignments. A one-alignment sample
returns that alignment without pairwise diagnostics.

# SHARED OPTIONS

**-h**, **`--help`**
: Show help. After a command, show help for that command.

**-s** _N_, **`--skip`** _N_
: Skip the first _N_ alignments in each sample file. Default: 0. Does not affect the reference.

**-m** _N_, **`--max`** _N_
: Retain at most _N_ alignments per sample file, thinning the sample as needed. This does not
  simply take the first _N_ alignments. Default: 1000; `-1` retains all alignments after skipping.
  Does not affect the reference.

**-V**, **`--verbose`**
: Write additional progress messages to standard error.

**`--alphabet`** _ALPHABET_
: Specify DNA, RNA, Amino-Acids, Amino-Acids+stop, Triplets, Codons, or Codons+stop.
  By default, infer the alphabet from the input.

**`--distances`** _MEASURES_
: Select the measure. For `score`, separate multiple names with colons, for example
  `--distances=recall:accuracy`. Defaults are listed under COMMANDS.

# REPORTING OPTIONS

These options follow `compare` or `distances`.

**`--CI`** _P_
: Central interval probability. Default: 0.95, giving the 2.5th to 97.5th percentiles.

**`--mean`**
: Show mean and standard deviation.

**`--median`**
: Show median and central interval. Used by default when none of the three report flags is given.

**`--minmax`**
: Show minimum and maximum.

# EXAMPLES

Measure recovery of reference residue pairs:

```
alignment-distances score reference.fasta sample.fastas --distances=recall:accuracy
```

Write an alignment-distance matrix:

```
alignment-distances AxA sample.fastas > distances.tsv
```

Locate disagreement by sequence pair:

```
alignment-distances NxN reference.fasta sample.fastas > sequence-distances.tsv
```

Select a representative alignment after discarding 100 sampled alignments:

```
alignment-distances median --skip=100 sample.fastas > representative.fasta
```

Compare two samples using a directional distance:

```
alignment-distances compare --mean --distances=nonrecall sample1.fastas sample2.fastas
```

Summarize an entire sample, without thinning:

```
alignment-distances distances --max=-1 --median --minmax sample.fastas
```

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
