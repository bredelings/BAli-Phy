% extract-ancestors(1)
% Benjamin Redelings
% Feb 2018

# NAME

**extract-ancestors** - Construct alignments with internal sequences for labeled nodes in query tree.

# SYNOPSIS

**extract-ancestors** _alignments file_ _trees file_ _alignment file_ [OPTIONS]

# DESCRIPTION

Construct alignments with internal sequences for labeled nodes in query tree.

# GENERAL OPTIONS:
**-h**, **--help**
: produces help message

**-v**, **--verbose**
: \[=arg(=1)\]      Show more log messages on stderr.


# ALLOWED OPTIONS:
**-A** _arg_, **--alignments** _arg_
: File of alignment samples

**--alphabet** _arg_
: set to 'Codons' to prefer codon alphabets

**-T** _arg_, **--trees** _arg_
: File of corresponding tree samples

**-x** _arg_ (=10), **--subsample** _arg_ (=10)
: factor by which to sub-sample trees


# ANCESTOR OPTIONS:
**-n** _arg_, **--nodes** _arg_
: Newick tree with labelled ancestors

**-g** _arg_, **--groups** _arg_
: File with named groups


# OUTPUT OPTIONS:
**-a** _arg_, **--template-alignment** _arg_
: File with template alignment

**--show-ancestors** _arg_ (=0)
: Write input alignments augmented with ancestor sequences.


# EXAMPLES:
 
```
% extract-ancestors -A C1.P1.fastas -T C1.trees -a P1-max.fasta --nodes query.tree --groups query.tree
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

