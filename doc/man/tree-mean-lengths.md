% tree-mean-lengths(1)
% Benjamin Redelings
% Feb 2018

# NAME

**tree-mean-lengths** - Compute the mean lengths for branches in the given topology.

# SYNOPSIS

**tree-mean-lengths** [OPTIONS] _tree_ _file1_ [_file2_ ...]

# DESCRIPTION

Compute the mean lengths for branches in the given topology.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**-T** _arg_, **--tree** _arg_
: tree to re-root

**--files** _arg_
: tree samples to examine

**-s** _arg_ (=0), **--skip** _arg_ (=0)
: number of tree samples to skip

**-u** _arg_, **--until** _arg_
: Read until this number of trees.

**--ignore** _arg_
: Comma-separated taxa to remove

**--simple**
: Ignore all branches not in the query tree

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: factor by which to sub-sample

**--var**
: report standard deviation of branch lengths instead of mean

**--no-node-lengths**
: ignore branches not in the specified topology

**--safe**
: Don't die if no trees match the topology

**--show-node-lengths**
: Output special format

**-V**, **--verbose**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

