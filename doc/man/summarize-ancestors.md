% summarize-ancestors(1)
% Benjamin Redelings
% Feb 2018

# NAME

**summarize-ancestors** - Construct alignments with internal sequences for labeled nodes in query tree.

# SYNOPSIS

**summarize-ancestors** _alignment_ -A _fastas1_ -T _trees1_ [OPTIONS]

# DESCRIPTION

Construct alignments with internal sequences for labeled nodes in query tree.

# GENERAL OPTIONS:
**-h**, **--help**
: produces help message

**-V**, **--verbose**
: \[=arg(=1)\]   Show more log messages on stderr.


# INPUT OPTIONS:
**-A** _arg_, **--alignments** _arg_
: File of alignment samples

**-T** _arg_, **--trees** _arg_
: File of corresponding tree samples

**-x** _arg_ (=10), **--subsample** _arg_ (=10)
: factor by which to sub-sample trees

**-m** _arg_ (=500), **--max** _arg_ (=500)
: Thin (alignment,tree) pairs down to this number of samples.


# ANCESTOR QUERY OPTIONS:
**-n** _arg_, **--nodes** _arg_
: Newick tree with labelled ancestors

**-g** _arg_, **--groups** _arg_
: File with named groups

**--nodes-min** _arg_ (=0.34)
: Minimum fraction to include a node.

**--groups-min** _arg_ (=0.34)
: Minimum fraction to include a group.


# EXAMPLES:
 
Add ancestral sequences to summary alignment:
```
% summarize-ancestors summary.fasta -A C1.P1.fastas -T C1.trees --nodes query.tree --groups query.tree
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

