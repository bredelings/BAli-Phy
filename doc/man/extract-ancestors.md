% extract-ancestors(1)
% Benjamin Redelings
% Feb 2018

# NAME

**extract-ancestors** - Extract and name ancestral sequences according to node- and branch- queries.

# SYNOPSIS

**extract-ancestors** [OPTIONS]

# DESCRIPTION

Extract and name ancestral sequences according to node- and branch- queries.

# GENERAL OPTIONS:
**-h**, **--help**
: produces help message

**-V**, **--verbose**
: Set diagnostic verbosity on stderr; omitted value means 1.


# INPUT OPTIONS:
**--alphabet** _name_
: Sequence alphabet; inferred from input when omitted.

**-A** _arg_, **--alignments** _arg_
: File of alignment samples

**-T** _arg_, **--trees** _arg_
: File of corresponding tree samples

**-x** _arg_ (=10), **--subsample** _arg_ (=10)
: factor by which to sub-sample trees


# ANCESTOR QUERY OPTIONS:
**-n** _arg_, **--nodes** _arg_
: Newick tree with labelled ancestors

**-g** _arg_, **--groups** _arg_
: File with named groups

# EXAMPLES:
 
Add named ancestral sequences to alignments where they are present:
```
% extract-ancestors -A C1.P1.fastas -T C1.trees --nodes query.tree --groups query.tree
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

