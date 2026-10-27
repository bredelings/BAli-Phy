% alignment-smc(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-smc** - Filter alignments, summarize variation, and prepare SMC input.

# SYNOPSIS

**alignment-smc** _COMMAND_ _ARGUMENTS_ [OPTIONS]

# DESCRIPTION

**alignment-smc** filters aligned sequences, summarizes variation among them, and prepares
input for sequentially Markovian coalescent (SMC) programs used to infer population history.
Choose one command to write a processed alignment, report variation, or prepare input for
MSMC, PSMC, or DiCal2. These commands share the preprocessing options below, so analyses
can use the same filtered or masked alignment.

Two separate commands use aligned sequence pairs to translate mask or VCF coordinates
between reference sequences. They do not apply the alignment preprocessing options.

Results go to standard output; diagnostics go to standard error. Use **-h** or **--help**
after a command to see its options. An alignment filename of `-` reads standard input.

# COMMANDS

**show** _ALIGNMENT_
: Write the processed alignment. Also report variation and masking statistics to standard
  error.

**find-alleles** _ALIGNMENT_ **--snps** _N_ **--length** _L_
: Report alleles differing from a consensus at _N_ consecutive SNPs spanning at most _L_
  reference positions, inclusive. Both _N_ and _L_ must be at least 2. The table gives
  occurrence count, 1-based start, inclusive length, consensus, and allele. Coordinates
  refer to the first sequence. Reporting uses the alignment after all preprocessing,
  including allele masking, and recomputes its consensus from that alignment.

    **--min-count** _N_ (default: 1) and **--max-count** _N_ (default: number of sequences)
    restrict the allele occurrence count. An allele here is a pattern of exact letters at
    the same _N_ differing columns, counted among the sequences selected by
    **--consensus-seqs**. The consensus and allele strings contain only those _N_ letters,
    not the intervening bases. Only differences between exact letters are counted.

**snp-distances** _ALIGNMENT_ _MAX_SNPS_
: Report distances between differing exact letters in successive sequence pairs: the
  first and second sequences, then the third and fourth, and so on. If the sequence
  count is odd, the last sequence is paired with the first. For each pair, consider
  endpoints separated by 1 through _MAX_SNPS_ SNP intervals; 1 means adjacent SNPs.
  At separation _J_, successive reported spans advance by _J_ SNPs.

    The tab-separated columns are `seq1`, `seq2` (0-based sequence indices), `SNPs`
    (endpoint separation in SNP intervals), `ref_pos` (0-based start in the first
    sequence of the entire alignment), and `distance` (distance along the first
    sequence of the pair). Sequence coordinates count non-gap characters.

**histogram** _ALIGNMENT_ _BLOCK_SIZE_
: Write one SNP count per consecutive block of _BLOCK_SIZE_ alignment columns (a
  positive integer), in alignment order. Include a final partial block. The output
  is a list of block counts, not a frequency table of those counts.

**sfs2d** _ALIGNMENT_ _POP1_ _POP2_ _ANCESTOR_
: Report a two-population derived-allele count spectrum across the entire alignment.
  The three arguments are sequence-name prefixes identifying the two populations and
  ancestral sequences. Sites with uncalled members in either population are excluded;
  the ancestor must have exactly one observed allele. Only sites with two observed
  alleles across the populations count. Each output row gives the two derived-allele
  counts and the number of sites with those counts. A derived allele is the letter
  differing from the observed ancestral allele; its count is the number of sequences
  carrying it in that population.

**select-sites** _ALIGNMENT_ **--minor-allele** _COUNT_
: Write an alignment containing columns with minor-allele count at least _COUNT_.
  This is the second-largest count of an exact letter at the site, including at sites
  with more than two alleles.

    **--missing-freq** _F_ (default: 1) limits the missing frequency.
    **--one-every** _N_ retains the first qualifying column in each fixed bin of _N_
    positions (positive); it does not guarantee that retained sites are _N_ positions
    apart. Bins have 0-based exclusive ends `N-1`, `2N-1`, and so on.

    **--write-bed** _CHROMOSOME_ writes a BED-style table instead of an alignment.
    The header is `#CHROM POS ID REF ALT`: the first three fields actually contain
    the chromosome name, 0-based alignment-column start, and exclusive end (start + 1).
    `REF` and `ALT` are the most frequent and second-most frequent exact letters across
    all sequences; `REF` is not necessarily the letter in the reference sequence.

    With this table, **--show-freq** adds `FREQ` for all sequences and `FREQ_population`
    for each population identified by names of the form `population:sequence`.
    Frequencies are percentages of exact observations carrying `ALT`, rounded down to
    integers. Gaps, missing calls, and ambiguities do not contribute to the denominator;
    `NA` indicates no exact observations. Population columns appear in name order.

**pi-matrix** _ALIGNMENT_
: Write a tab-separated matrix of pairwise diversity (pi). Each entry is the fraction
  of differing letters among columns with exact observations in both sequences. Rows
  and columns follow input sequence order, with no labels; diagonal entries are zero.
  A pair with no comparable observations has an undefined value (NaN).

**msmc** _ALIGNMENT_
: Write one MSMC record per unmasked variant site. Its four fields are chromosome
  label (always `1`), 1-based alignment position, count of unmasked sites since the
  preceding emitted SNP (including the current site), and the letters in input sequence
  order. Masked sites contribute zero to the count; unmasked sites on either side of
  a mask still contribute.

**psmc** _ALIGNMENT_
: Write PSMC symbols for the first two sequences in complete 1,000-position blocks.
  A block is `K` if it contains an observed difference, `T` if it has callable positions
  but no difference, and `N` if no position has two exact observed letters. The trailing
  partial block is omitted.

**dical2** _ALIGNMENT_
: Write binary haplotypes for DiCal2. Silently omit sites with more than two observed
  exact alleles, preserving the coordinates of retained sites. Output gives the
  alignment length, 1-based segregating-site positions, and one haplotype per sequence
  in input order. At each retained site, `0` denotes a match to the first sequence
  and `1` denotes a different value.

**translate-mask** _ALIGNMENT_ _MASK_
: Translate a mask file from the first sequence to the second using their pairwise
  alignment. Exactly two sequences are required. Both input and output use 0-based
  inclusive coordinates among non-gap characters. Ambiguous and missing bases still
  occupy positions. Each interval covers the target bases within the alignment span
  of its first and last source bases, including target insertions inside that span.
  It does not expand to neighboring bases outside the span. Intervals containing no
  target bases are omitted. **--alphabet** specifies the alignment alphabet if needed.

**translate-vcf** _ALIGNMENTS_ _VCF_
: Translate 1-based positions in a VCF file using consecutive source/target sequence
  pairs in a FASTA file. Each pair must be aligned, but different pairs may have
  different lengths.
  Source sequence names identify VCF chromosomes. Records at source bases deleted
  from the target are discarded with a diagnostic. Chromosomes with no translation
  table pass through unchanged. Translated records use the target sequence name and
  position; allele strings and all other VCF fields remain unchanged. _VCF_ may be `-` to
  read standard input; only one input can use standard input.

The **show**, **msmc**, **psmc**, and **dical2** commands also write summary statistics to
standard error. Their **--variant** _N_ option (default: 1) sets the SNP separation used
in the summary's count of variant sites preceding another variant.

# PREPROCESSING OPTIONS

These options belong to every command except **translate-mask** and **translate-vcf**.
Operations run in this order, regardless of option order: empty-column removal,
reference cleaning, automatic masking, mask files, gap masking/removal, and allele
masking. Masking hides observations; removing columns changes the alignment's positions.
Alignment-column coordinates include gap columns. Reference-sequence coordinates count
positions along the named sequence instead. Coordinates refer to the alignment at the
stage where the operation runs, so removing columns can affect later coordinates.

**--alphabet** _NAME_
: Specify the alphabet: DNA, RNA, Amino-Acids, Triplets, or Codons. By default it is
  inferred from the alignment.

**-e**, **--erase-empty-columns**
: Remove empty columns, including columns consisting entirely of gaps or unknown
  gap-or-base calls (`?`). By default, empty columns are retained to preserve coordinates.

**--clean-to-ref** _NAME_
: Remove columns absent from the named reference sequence.

**-A**, **--autoclean**
: Mask regions of unusually dense variation. Choose a sliding-window length by
  rounding twice the alignment length divided by its SNP count, giving approximately
  two SNPs per window at the overall density. Windows containing at least 20 SNPs
  trigger masking of the window and one preceding column. Require at least 100
  variant sites in the alignment.

**-M** _FILE_, **--mask-file** _FILE_
: Apply a file of 0-based inclusive intervals, such as `10 - 20`, under a `>sequence-name`
  header. Coordinates refer to the named reference's sequence positions and are mapped
  to alignment columns; deletions in other sequences do not shift them. The corresponding
  columns are masked across all sequences. May be repeated.

**-G** _N_, **--mask-gaps** _N_
: Mask columns within _N_ columns of a gap.

**--gap-fraction** _F_ (default: 0.01)
: Fraction of sequences that must have a gap for **--mask-gaps** to treat a column as
  a gap column.

**-S**, **--strip-gaps**
: With **--mask-gaps**, remove gap columns and mask nearby columns.

**-m** _N:L_, **--mask-alleles** _N:L_
: Mask alleles with _N_ consecutive differences from the consensus spanning at most
  _L_ alignment columns, inclusive. Mask the entire span in each affected sequence,
  rather than across all sequences. Both _N_ and _L_ must be at least 2. May be repeated
  for different SNP counts and spans; all specifications use the same consensus.

**--consensus-seqs** _RANGE_ (default: all sequences)
: Select sequences for the consensus used by **--mask-alleles** and **find-alleles**.
  Use 1-based indices and ranges, for example `1-3,5`. **find-alleles** also searches
  and counts alleles only in these sequences.

    Reporting uses a majority consensus: a majority of gaps gives a gap; otherwise an
    exact letter must have a majority among counted non-gap observations. Without such
    a letter, the consensus is missing and cannot contribute a reported SNP. Masking
    instead chooses the most frequent exact letter when counted non-gap observations
    outnumber gaps, even without a letter majority; otherwise it uses a gap. Counted
    non-gap observations comprise exact letters and missing bases known to be present
    (`N` in DNA). If none is exact, the non-gap consensus remains missing.

# EXAMPLES

Write a masked alignment:

```
alignment-smc show sequences.fasta --mask-file regions.mask > masked.fasta
```

Write SNPs with minor-allele count at least 2 in BED format:

```
alignment-smc select-sites sequences.fasta --minor-allele 2 --write-bed chrom > snps.bed
```

Count SNPs in successive 1,000-column blocks:

```
alignment-smc histogram sequences.fasta 1000 > snp-counts.txt
```

Translate a mask through a reference/target alignment:

```
alignment-smc translate-mask pair.fasta reference.mask > target.mask
```

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
