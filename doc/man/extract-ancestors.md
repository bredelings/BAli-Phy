% extract-ancestors(1)
% Benjamin Redelings
% Feb 2018

# NAME

**extract-ancestors** - Construct alignments with internal sequences for labeled nodes in query tree.

# SYNOPSIS

**extract-ancestors** _alignments file_ _trees file_ _query tree_[OPTIONS]

# DESCRIPTION

Construct alignments with internal sequences for labeled nodes in query tree.

# ALLOWED OPTIONS:
**-h**, **--help**
: produces help message

**-V**, **--verbose**
: \[=arg(=1)\]   Show more log messages on stderr.

**-a** _arg_, **--alphabet** _arg_
: set to 'Codons' to prefer codon alphabets

**-x** _arg_ (=10), **--subsample** _arg_ (=10)
: factor by which to sub-sample trees

**-L** _arg_ (=1), **--show-leaves** _arg_ (=1)
: Include leaf sequences in filtered alignments

**-A** _arg_ (=0), **--all-nodes** _arg_ (=0)
: Only show alignments with ALL the labeled internal nodes


# EXAMPLES:
 
```
% extract-ancestors C1.P1.fastas C1.trees query.tree
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

