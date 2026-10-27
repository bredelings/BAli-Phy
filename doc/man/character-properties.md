% character-properties(1)
% Benjamin Redelings
% October 2026

# NAME

**character-properties** - Summarize and report sampled character properties.

# SYNOPSIS

**character-properties summarize** _SAMPLE-FILE_ \[...\] \[OPTIONS\]

**character-properties columns** _SUMMARY_ _ALIGNMENT_ _PROPERTY_ \[OPTIONS\]

**character-properties letters** _SUMMARY_ _ALIGNMENT_ _PROPERTY_ \[OPTIONS\]

**character-properties positive-selection** _SUMMARY_ _ALIGNMENT_ \[_PROPERTY_\] \[OPTIONS\]

The **letters** command requires exactly one named selection option: **`--above`**, **`--below`**,
**`--highest`**, or **`--lowest`**. Reporting options follow the subcommand name.

# DESCRIPTION

**character-properties** summarizes posterior samples of properties such as substitution rates
or indicators of positive selection, then displays those summaries on an alignment of your choice.
Properties belong to individual sequence characters: a character is a nucleotide, amino acid,
or codon, according to the alphabet. Changing the display alignment changes which characters
share a column, not the posterior summary attached to each character.

Choose a command according to the result needed:

- **summarize** reads sampled properties and writes a reusable JSON summary.
- **columns** describes the range and middle of the character summaries within each column.
- **letters** selects characters by their property values and reports one representative per column.
- **positive-selection** reports probabilities of positive selection and associated dN/dS summaries.

The three reporting commands read a summary produced by **summarize** and a template alignment.
Both **columns** and **letters** produce at most one row per alignment column; **letters** does
not list every character separately. Results go to standard output and diagnostics to standard error.

Rate properties use the substitution model's own scale, before scaling inside `PhyloCTMC`.
They do not necessarily average to one.

# INPUT AND SUMMARIZATION

Supply one JSON Lines sample file per chain to **summarize**, normally named
`C1.P1.site-property-samples.jsonl` for partition 1. Retained records must describe the same
properties, sequence names, and ungapped character counts across samples and chains.
Iterations must be nonnegative and strictly increasing within each chain.

Samples are pooled by retained draw, so a chain with more retained samples contributes more
weight. The summary contains each character's posterior mean, population standard deviation,
and exact median. For an even sample count, the median is the lower of the two central values.
The JSON document identifies characters by sequence name and zero-based ungapped character index.

Summarization can run while sample files are growing. Only newline-terminated records are read;
an incomplete final record is ignored. Later passes use the file prefixes captured in the first
pass, ignoring subsequent additions and rejecting changes to the captured data. Exact medians
may require repeated reads; **`--median-memory`** controls the working-memory budget.

For reporting, use sequence names and ungapped characters corresponding to the summarized data.
The template may change gap placement or display a subset of the sequences. Columns containing
no observed characters are omitted. Reported column numbers and ungapped character positions
are one-based and measured in alphabet units: with Codons, they count codons, not nucleotides.

# REPORT FORMATS

Reports default to readable text. **`--format=tsv`** writes a header followed by tab-separated rows,
without explanatory prose. The formats below describe the numerical content of each report.

## Column summaries

**columns** reports these eight fields:

- `column`: position in the template alignment.
- `letters`: number of observed characters in that column.
- `mean-min`, `mean-middle`, `mean-max`: minimum, lower middle, and maximum of those characters'
  posterior means.
- `median-min`, `median-middle`, `median-max`: the corresponding summaries of their posterior medians.

The middle value is the lower central observation when a column contains an even number of
characters. It summarizes differences between characters in the column; it is distinct from a
character's posterior median across samples.

## Representative letters

**letters** reports these eight fields:

- `column`: position in the template alignment.
- `sequence`, `sequence-character`: the representative's sequence name and ungapped position.
- `symbol`, `translation`: its observed symbol and, for codons, amino-acid translation.
- `mean`, `sd`, `median`: its posterior summary values.

The SD describes posterior variation for that character, not variation between letters in the
column or the standard error of its estimated mean. TSV translation fields are empty for
non-codon alphabets.

Selection is global over the characters in the template alignment. Among selected characters
in a column, **`--above`** and **`--highest`** choose the highest-scoring representative;
**`--below`** and **`--lowest`** choose the lowest-scoring representative. Sorting happens afterward.
Thus selecting 5% of characters need not report 5% of columns. Even **`--highest=100%`** produces
representatives rather than the ranges reported by **columns**.

## Positive selection

**positive-selection** reports one representative per selected column. Its default property is
`posSelection`, with associated `dNdS` summaries when present. A property ending in
`-posSelection`, such as `foreground-posSelection`, uses the corresponding `-dNdS` property.
Probabilities describe posterior support for dN/dS being greater than one.

By default, the report pairs model-averaged values with values conditioned on
`positiveSelectionInModel` being true. Model-averaged values use all retained samples;
conditioned values use only matching samples. **`--condition`** chooses another condition,
and **`--unconditional`** requests model-averaged values alone.

For threshold selection, the representative is the character with the highest conditioned
probability in the column, or the highest model-averaged probability when no conditioned samples
are available. The column is retained if either probability for that representative exceeds
the threshold. Percentage selection ranks characters by conditioned probability when available,
otherwise by model-averaged probability. Paired values always describe the same character;
dN/dS values do not affect representative choice.

TSV rows begin with `column`, `sequence`, `sequence-character`, `symbol`, and `translation`.
They then contain `model-averaged-probability`, `model-averaged-dNdS-mean`,
`model-averaged-dNdS-sd`, `model-averaged-dNdS-median`, and the four corresponding
`conditioned-*` fields. Unavailable values have empty fields. Text reports show the available
posterior views and dN/dS means and SDs in a compact layout.

A missing requested condition is an error, including the default positive-selection condition;
use **`--unconditional`** if only model-averaged results are wanted. If the condition exists but
has no matching samples, **positive-selection** uses model-averaged selection and leaves
conditioned values unavailable. In contrast, **columns** and **letters** report an error when
an explicitly requested condition has no matching samples.

# OPTIONS

**-h**, **`--help`**
: Show help for the program or selected subcommand, for example
  `character-properties letters --help`.

## Summarize options

**`--skip=ITER`**
: Discard records before iteration _ITER_. The lower bound is inclusive; omitted by default.

**`--until=ITER`**
: Discard records after iteration _ITER_. The upper bound is inclusive; omitted by default.
  It must be at least **`--skip`** when both are specified.

**`--subsample=N`**
: Retain the first eligible stored record and every *N*th eligible record thereafter,
  independently in each chain, after applying the iteration bounds. _N_ must be positive
  (default: 1).

**`--median-memory=MIB`**
: Positive target in mebibytes for exact-median working memory (default: 256).
  This excludes the decoded input record and final summary arrays.

## Reporting options

These options belong to **columns**, **letters**, and **positive-selection**, except where noted.

**`--alphabet=ALPHABET`**
: Constrain the alphabet used to interpret characters. **columns** and **letters** guess it
  from the alignment by default; **positive-selection** defaults to Codons. Partial names
  are completed from the data: `Codons` guesses DNA versus RNA and uses the standard genetic
  code; `Codons(,mt-vert)` guesses DNA versus RNA but uses the vertebrate mitochondrial code.

**`--format=text|tsv`**
: Choose readable text or tab-separated output (default: text).

**`--sort=column|increasing|decreasing`**
: Order completed rows (default: column). For **columns**, score order uses the column's
  middle value for the statistic selected by **`--by`**. For **letters**, it uses the
  representative's selected statistic. **positive-selection** sorts by model-averaged
  probability. Equal scores have no defined secondary order.

**`--condition=NAME`**
: For **columns** and **letters**, use only the posterior conditioned on the named Boolean
  being true; the default is model-averaged reporting. For **positive-selection**, choose
  the conditioned posterior paired with model-averaged results. Cannot be combined with
  **`--unconditional`**.

**`--by=mean|median`**
: Available on **columns** and **letters** (default: mean). Choose the statistic for sorting
  columns, or for selecting and sorting letters. This does not remove other statistics
  from the output table.

## Letter selection options

Exactly one of these options is required by **letters**. None is accepted by **columns**.

**`--above=VALUE`**, **`--below=VALUE`**
: Select characters whose scores are strictly greater or strictly less than _VALUE_, respectively.

**`--highest=PERCENT`**, **`--lowest=PERCENT`**
: Select the highest- or lowest-scoring percentage of characters. An explicit value ending
  in `%` is required, for example **`--highest=5%`**. Percentages must be greater than 0% and
  at most 100%. With _N_ characters, select `max(1, floor(N*PERCENT/100))`; ties do not enlarge
  this count and have no defined secondary order. Multiple selected characters can share a column.

## Positive-selection options

**`--above=PROBABILITY`**
: Retain a column when either probability at its representative is strictly above this
  threshold (default: 0.5). The threshold must be between 0 and 1.

**`--highest=PERCENT`**
: Select a percentage of characters by probability instead of applying a threshold.
  The explicit `%` value, range, rounding, and tie rules are the same as for **letters**.
  Mutually exclusive with **`--above`**.

**`--unconditional`**
: Report and select from the model-averaged posterior alone. Mutually exclusive with **`--condition`**.

# EXAMPLES

Pool two chains from iteration 1000 onward:

    character-properties summarize \
        run-1/C1.P1.site-property-samples.jsonl \
        run-2/C1.P1.site-property-samples.jsonl \
        --skip=1000 > properties.json

Describe every nonempty alignment column for a rate property:

    character-properties columns properties.json alignment.fasta rate

Select letters with posterior median rates above 2, showing the highest scores first:

    character-properties letters properties.json alignment.fasta rate \
        --above=2 --by=median --sort=decreasing

Report the highest 1% of letters, retaining at least one when the alignment has characters:

    character-properties letters properties.json alignment.fasta rate \
        --highest=1%

Report positive-selection support with a probability threshold of 0.95:

    character-properties positive-selection \
        codon-properties.json codons.fasta --above=0.95

Report model-averaged foreground selection using the vertebrate mitochondrial genetic code:

    character-properties positive-selection \
        foreground-properties.json codons.fasta foreground-posSelection \
        --unconditional --alphabet='Codons(,mt-vert)'

# SEE ALSO

**alignment-draw**(1), **bpy-summarize**(1)

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
