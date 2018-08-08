% alignment-find(1)
% Benjamin Redelings
% Feb 2018

# NAME

**alignment-find** - Find conserved parts of an alignment.

# SYNOPSIS

**alignment-find** conserved _alignment-file_ [OPTIONS]

# DESCRIPTION

Find conserved parts of an alignment.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--align** _arg_ (=-)
: file with alignment to convert (default STDIN)

**--tree** _arg_
: file with initial tree

**--alphabet** _arg_
: specify the alphabet: DNA, RNA, Amino-Acids, Triplets, or Codons

**--groups** _arg_
: file with taxon groups

**--split** _arg_
: a split to consider

**--ignore-rate-change**
: ignore rate changes

**--require-conservation**
: Highlight conserved regions

**--require-change**
: Highlight regions if conservation changes

**--require-all-different**
: Taxa differences are interesting of there is no letter overlap


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

