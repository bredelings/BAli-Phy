% alignment-smc(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-smc** - Generate input for SMC programs.

# SYNOPSIS

**alignment-smc** _alignment-file_ [OPTIONS]

# DESCRIPTION

Generate input for SMC programs.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--align** _arg_
: file with sequences and initial alignment

**--alphabet** _arg_
: specify the alphabet: DNA, RNA, Amino-Acids, Triplets, or Codons

**-e**, **--erase-empty-columns**
: Remove columns with no characters (all gaps).

**--minor-allele** _arg_
: Keep columns with minor-allele count >= _arg_

**--missing-freq** _arg_ (=1)
: Keep columns with missing frequency >= _arg_

**--one-every** _arg_
: Keep only 1 column in each interval of size _arg_

**-G** _arg_, **--mask-gaps** _arg_
: Remove columns within _arg_ columns of a gap

**-S**, **--strip-gaps**
: Remove columns within _arg_ columns of a gap

**--gap-fraction** _arg_ (=0.01)
: Fraction of sequences that need to have a gap

**-M** _arg_, **--mask-file** _arg_
: Apply mask-file

**-A**, **--autoclean**
: Mask blocks with too many SNPs

**--dical2**
: Output file for DiCal2

**--msmc**
: Output file for MSMC

**--psmc**
: Output file for PSMC

**--write-bed** _arg_
: Output BED file with chromosome name _arg_

**--variant** _arg_ (=1)
: Is there a SNP at distance _arg_ from SNP?

**--consensus-seqs** _arg_
: sequences to use in consensus

**--translate-mask** _arg_
: Masks (CSV or @file)

**--translate-vcf** _arg_
: Masks (CSV or @file)

**--show-freq**
: Show allele frequencies

**--clean-to-ref** _arg_
: Remove columns not in reference sequence _arg_

**--pi-matrix**
: Calculate pi for each pair of sequences

**--histogram** _arg_
: Output SNP counts for blocks of arg bases

**--snp-snp-lengths** _arg_
: Output counts of snp-snp lengths up to arg snps

**--sfs2d** _arg_
: pop1:pop2:anc:window

**--find-alleles** _arg_
: Find alleles with S snps in L bases and count >= N

**-m** _arg_, **--mask-alleles** _arg_
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

