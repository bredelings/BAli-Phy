% alignment-smc(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-smc** - Filter alignments, summarize variation, and prepare SMC input.

# SYNOPSIS

**alignment-smc** _COMMAND_ _ARGUMENTS_ [OPTIONS]

# DESCRIPTION

**alignment-smc** combines alignment filtering and variation summaries with output formats
for sequentially Markovian coalescent (SMC) programs. Choose one command to write an alignment,
report variation, or prepare input for MSMC, PSMC, or DiCal2. These commands share the
preprocessing options below, so analyses can use the same filtered or masked alignment.

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
    restrict the allele occurrence count.

**snp-distances** _ALIGNMENT_ _MAX_SNPS_
: Report distances between SNPs, considering endpoints separated by up to _MAX_SNPS_
  SNP intervals.

**histogram** _ALIGNMENT_ _BLOCK_SIZE_
: Report SNP counts in blocks of _BLOCK_SIZE_ positions (a positive integer).

**sfs2d** _ALIGNMENT_ _POP1_ _POP2_ _ANCESTOR_
: Report a two-population derived-allele count spectrum across the entire alignment.
  The three arguments are sequence-name prefixes identifying the two populations and
  ancestral sequences. Sites with uncalled members in either population are excluded;
  the ancestor must have exactly one observed allele. Only sites with two observed
  alleles across the populations count. Each output row gives the two derived-allele
  counts and the number of sites with those counts.

**select-sites** _ALIGNMENT_ **--minor-allele** _COUNT_
: Write an alignment containing columns with minor-allele count at least _COUNT_.

    **--missing-freq** _F_ (default: 1) limits the missing frequency.
    **--one-every** _N_ keeps one column per interval of _N_ positions (positive).
    **--write-bed** _CHROMOSOME_ writes a BED table instead of an alignment.
    With BED output, **--show-freq** adds alternate-allele frequencies as integer
    percentages of exact observations. Gaps, missing calls, and ambiguities do not
    contribute to the denominator; `NA` indicates no exact observations.

**pi-matrix** _ALIGNMENT_
: Write a tab-separated matrix of pairwise nucleotide diversity (pi).

**msmc** _ALIGNMENT_
: Write MSMC records with 1-based positions and counts of unmasked sites since the
  preceding emitted SNP, including the current site. Masked sites contribute zero;
  unmasked sites on either side of a mask still contribute.

**psmc** _ALIGNMENT_
: Write PSMC symbols for the first two sequences in complete 1,000-position blocks.
  A block is `K` if it contains an observed difference, `T` if it has callable positions
  but no difference, and `N` if no position has two exact observed letters. The trailing
  partial block is omitted.

**dical2** _ALIGNMENT_
: Write binary haplotypes for DiCal2. Silently omit sites with more than two observed
  exact alleles, preserving the coordinates of retained sites.

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
  pairs. Each pair must be aligned, but different pairs may have different lengths.
  Source sequence names identify VCF chromosomes. Records at source bases deleted
  from the target are discarded with a diagnostic. Chromosomes with no translation
  table pass through unchanged. Other VCF fields are preserved. _VCF_ may be `-` to
  read standard input; only one input can use standard input.

The **show**, **msmc**, **psmc**, and **dical2** commands also write summary statistics to
standard error. Their **--variant** _N_ option (default: 1) sets the SNP separation used
in the summary's count of variant sites preceding another variant.

# PREPROCESSING OPTIONS

These options belong to every command except **translate-mask** and **translate-vcf**.
Operations run in this order, regardless of option order: empty-column removal,
reference cleaning, automatic masking, mask files, gap masking/removal, and allele
masking. Masking hides observations; removing columns changes the alignment's positions.

**--alphabet** _NAME_
: Specify the alphabet: DNA, RNA, Amino-Acids, Triplets, or Codons. By default it is
  inferred from the alignment.

**-e**, **--erase-empty-columns**
: Remove empty columns, including columns consisting entirely of gaps or unknown
  gap-or-base calls (`?`). By default, empty columns are retained to preserve coordinates.

**--clean-to-ref** _NAME_
: Remove columns absent from the named reference sequence.

**-A**, **--autoclean**
: Automatically mask blocks with too many SNPs.

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
  _L_ alignment columns, inclusive. May be repeated for different SNP counts and spans.

**--consensus-seqs** _RANGE_ (default: all sequences)
: Select sequences for the consensus used by **--mask-alleles** and **find-alleles**.
  Use 1-based indices and ranges, for example `1-3,5`.

# EXAMPLES

Write a masked alignment:

```
alignment-smc show sequences.fasta --mask-file regions.mask > masked.fasta
```

Write SNPs with minor-allele count at least 2 in BED format:

```
alignment-smc select-sites sequences.fasta --minor-allele 2 --write-bed chrom > snps.bed
```

Translate a mask through a reference/target alignment:

```
alignment-smc translate-mask pair.fasta reference.mask > target.mask
```

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.
