% tree-mean-lengths(1)
% Benjamin Redelings
% Feb 2018

# NAME

**tree-mean-lengths** - Compute the mean lengths for branches in the given topology.

# SYNOPSIS

**tree-mean-lengths** _tree-file_ < in-file

# DESCRIPTION

Compute the mean lengths for branches in the given topology.

# ALLOWED OPTIONS:
**-h**, **--help**
: produce help message

**--tree** _arg_
: tree to re-root

**-s** _arg_ (=0), **--skip** _arg_ (=0)
: number of tree samples to skip

**-u** _arg_, **--until** _arg_
: Read until this number of trees.

**--prune** _arg_
: Comma-separated taxa to remove

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: factor by which to subsample

**--var**
: report standard deviation of branch lengths instead of mean

**--no-node-lengths**
: ignore branches not in the specified topology

**--safe**
: Don't die if no trees match the topology

**--drop-partial**
: Remove partial branches

**--seed** _arg_
: random seed

**-V**, **--verbose**
: Output more log messages on stderr.


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

