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

**-S**, **--strip-gaps**
: Remove columns within _arg_ columns of a gap

**-G** _arg_, **--mask-gaps** _arg_
: Remove columns within _arg_ columns of a gap

**-M** _arg_, **--mask-file** _arg_
: Apply mask-file

**--variant** _arg_ (=1)
: Is there a SNP at distance _arg_ from SNP?

**--dical2**
: Output file for DiCal2

**--msmc**
: Output file for MSMC

**--psmc**
: Output file for PSMC

**--autoclean**
: Mask blocks with too many SNPs

**--histogram** _arg_
: Output SNP counts for blocks of arg bases


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

