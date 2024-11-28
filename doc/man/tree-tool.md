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

**-V**, **--verbose**
: Output more log messages on stderr.


# MODIFICATION OPTIONS:
**--prune** _arg_
: Comma-separated taxa to remove

**--root**
: Find a root position

**--resolve**
: Resolve polytomies

**--remove-root-branch**
: Remove single branch from root.

**--remove-root-branches**
: Ensure root is not a tip.

**--remove-knuckles**
: Remove degree-2 nodes.

**--scale** _arg_
: Scale branch-lengths by factor

**--strip-internal-names**
: Remove internal node names

**--name-all-nodes**
: Add node names


# OUTPUT OPTIONS:
**--length**
: Report the total tree length

**--diameter**
: Report the tree diameter

**--count-leaves**
: Show the number of leaves

**--count-nodes**
: Show the number of nodes

**--show-leaves**
: Show the leaf names

**--show-nodes**
: Show the node names


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

