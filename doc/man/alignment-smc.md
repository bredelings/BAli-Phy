% alignment-smc(1)
% Benjamin Redelings
% October 2026

# NAME

**alignment-smc** - Generate input for SMC programs.

# SYNOPSIS

**alignment-smc** _alignment-file_ \[OPTIONS\]

# DESCRIPTION

Generate input for SMC programs.

# ALLOWED OPTIONS:
**-h**, **`--help`**
: produce help message

**`--align`** _arg_
: file with sequences and initial alignment

**`--alphabet`** _arg_
: specify the alphabet: DNA, RNA, Amino-Acids, Triplets, or Codons

**-e**, **`--erase-empty-columns`**
: Remove empty columns, including columns consisting entirely of gaps or unknown
  gap-or-base calls (`?`). By default, empty columns are retained to preserve coordinates.

**`--minor-allele`** _arg_
: Keep columns with minor-allele count >= _arg_

**`--missing-freq`** _arg_ (=1)
: With **`--minor-allele`**, keep columns with missing frequency <= _arg_.

**`--one-every`** _arg_
: With **`--minor-allele`**, keep one column per interval of size _arg_ (positive).

**-G** _arg_, **`--mask-gaps`** _arg_
: Mask columns within _arg_ columns of a gap

**-S**, **`--strip-gaps`**
: With **`--mask-gaps`**, remove gap columns and mask nearby columns. Takes no argument.

**`--gap-fraction`** _arg_ (=0.01)
: Fraction of sequences that need to have a gap

**-M** _arg_, **`--mask-file`** _arg_
: Apply a file of 0-based inclusive intervals, such as `10 - 20`, under a `>sequence-name`
  header. Coordinates refer to the named reference's sequence positions and are mapped to
  alignment columns; deletions in other sequences do not shift them. The corresponding
  columns are masked across all sequences. May be repeated.

**-A**, **`--autoclean`**
: Mask blocks with too many SNPs

**--dical2**
: Output binary haplotypes for DiCal2. Silently omit sites with more than two observed
  exact alleles, preserving the coordinates of retained sites.

**`--msmc`**
: Output MSMC records with 1-based positions and counts of unmasked sites since the
  preceding emitted SNP, including the current site. Masked sites contribute zero;
  unmasked sites on either side of a mask still contribute.

**`--psmc`**
: Output PSMC symbols for the first two sequences in complete 1,000-position blocks.
  A block is `K` if it contains an observed difference, `T` if it has callable positions
  but no difference, and `N` if no position has two exact observed letters. The trailing
  partial block is omitted.

**`--write-bed`** _arg_
: Output BED file with chromosome name _arg_

**`--variant`** _arg_ (=1)
: Is there a SNP at distance _arg_ from SNP?

**`--consensus-seqs`** _arg_
: sequences to use in consensus

**`--translate-mask`** _file_
: Translate a mask file from the first sequence to the second using their pairwise
  alignment. Both input and output use 0-based inclusive coordinates among non-gap
  characters. Ambiguous and missing bases still occupy positions. Each interval covers
  the target bases within the alignment span of its first and last source bases,
  including target insertions inside that span. It does not expand to neighboring bases
  outside the span. Intervals containing no target bases are omitted.

**`--translate-vcf`** _file_
: Translate 1-based positions in a VCF file using consecutive source/target sequence pairs
  in the alignment file. Source sequence names identify VCF chromosomes. Records at source
  bases deleted from the target are discarded with a diagnostic. Chromosomes with no
  translation table pass through unchanged. Other VCF fields are preserved.

**`--show-freq`**
: With **`--minor-allele`** and **`--write-bed`**, show alternate-allele frequencies as
  integer percentages of exact observations. Gaps, missing calls, and ambiguities do not
  contribute to the denominator. Report `NA` when no exact observations exist.

**`--clean-to-ref`** _arg_
: Remove columns not in reference sequence _arg_

**`--pi-matrix`**
: Calculate pi for each pair of sequences

**`--histogram`** _arg_
: Output SNP counts for blocks of _arg_ bases (positive).

**`--snp-snp-lengths`** _arg_
: Output counts of snp-snp lengths up to arg snps

**--sfs2d** _arg_
: Use `pop1:pop2:anc`, where each field is a sequence-name prefix. Report one
  two-population derived-allele count spectrum across the entire alignment. Sites with
  uncalled members in either population are excluded; the ancestor must have exactly
  one observed allele. Only sites with two observed alleles across the populations count.

**`--find-alleles`** _S:L:min:max_
: Find alleles with _S_ consecutive SNPs spanning at most _L_ reference positions,
  inclusive, and an occurrence count between _min_ and _max_. An empty _min_ defaults
  to 1; an empty _max_ defaults to the number of sequences. Report 1-based starts and
  inclusive lengths relative to the first sequence. Consensus letters are taken from
  the corresponding alignment columns.

**-m** _arg_, **`--mask-alleles`** _arg_
: Find alleles with S snps in L bases and count >= N


# EXAMPLES:
 
To calculate some statistics:
```
% alignment-smc sequence.fasta > /dev/null
```

To write out SNPS with minor-allele count >=2 in BED format:
```
% alignment-smc sequences.fasta --minor-allele=2 --write-bed=chrom > snps.bed
```

# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

