% alignment-gild(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-gild** - Annotate each residue in the alignment according to the probability that it should align to the hypothetical root character in its column.

# SYNOPSIS

**alignment-gild** alignment-file tree-file ... [OPTIONS] < alignments-file

# DESCRIPTION

Annotate each residue in the alignment according to the probability that it should align to the hypothetical root character in its column.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--align** _arg_
: file with sequences and initial alignment

**--tree** _arg_
: file with initial tree

**--find-root**
: estimate the root position from branch lengths

**--alphabet** _arg_
: set to 'Codons' to prefer codon alphabets

**--skip** _arg_ (=0)
: number of alignment samples to skip

**--max-alignments** _arg_ (=1000)
: maximum number of alignments to analyze

**-V**, **--verbose**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

