% tree-tool(1)
% Benjamin Redelings
% Feb 2018

# NAME

**tree-tool** - Perform various operations on Newick trees.

# SYNOPSIS

**tree-tool** _tree-file_ [OPTIONS]

# DESCRIPTION

Perform various operations on Newick trees.

# GENERAL OPTIONS:
**-h**, **--help**
: produce help message

**-v**, **--verbose**
: Output more log messages on stderr.


# MODIFICATION OPTIONS:
**--prune** _arg_
: Comma-separated taxa to remove

**--resolve**
: Comma-separated taxa to remove

**--remove-root-branch**
: Remove single branch from root.

**--remove-root-branches**
: Ensure root is not a tip.

**--remove-knuckles**
: Remove degree-2 nodes.


# OUTPUT OPTIONS:
**--scale** _arg_
: Scale branch-lengths by factor

**--length**
: Report the total tree length

**--diameter**
: Report the total tree length


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

