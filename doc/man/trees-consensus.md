% trees-consensus(1)
% Benjamin Redelings
% Feb 2018

# NAME

**trees-consensus** - Find consensus trees and supported splits.

# SYNOPSIS

**trees-consensus** [OPTIONS] _sampled-trees_ [_sampled-trees_ ... _sampled-trees_]

# DESCRIPTION

Find consensus trees and supported splits.

# INPUT OPTIONS:
**-h**, **--help**
: Produce help message.

**-V**, **--verbose**
: Output more log messages on stderr.

**-s** _arg_ (=10%), **--skip** _arg_ (=10%)
: Number of trees to skip.

**-u** _arg_, **--until** _arg_
: Read until this number of trees.

**-m** _arg_, **--max** _arg_
: Thin tree samples down to this number of trees.

**-x** _arg_ (=1), **--subsample** _arg_ (=1)
: Factor by which to subsample.

**-i** _arg_, **--ignore** _arg_
: Comma-separated list of taxa to ignore.


# REPORTING OPTIONS:
**--map-trees** _arg_ (=1)
: Only report the top _arg_ trees per file.

**--min-support** _arg_ (=0.25)
: Minimum threshold PP for splits.

**--report** _arg_
: Write supported partitions to file _arg_.

**--map-tree** _arg_
: Write out the map tree to file _arg_.

**--consensus** _arg_
: Write out consensus trees.

**--greedy-consensus** _arg_
: Write out greedy consensus trees.

**--extended-consensus-L** _arg_
: Write out extended consensus trees + lengths.

**--extended-consensus** _arg_
: Write out extended consensus trees.

**--support-levels** _arg_
: Write #branches versus LOD to file _arg_.

**--extended-support-levels** _arg_
: Write #sub-branches versus LOD to file _arg_.


# SEARCH OPTIONS:
**--sub-partitions**
: Search for partial splits.

**--depth** _arg_ (=1)
: Depth at which to look for partial splits.

**--rooting** _arg_ (=0.9)
: Threshold in search for partial splits.

**--odds-ratio** _arg_ (=1.5)
: Report partial-splits only if removing taxa improves the odds by at least this ratio.


# EXAMPLES:
 
Compute the majority consensus tree, skipping the first 10% of trees:
```
% trees-consensus newick.trees > c50.tree
% trees-consensus newick.trees --skip=10% > c50.tree
```

Skip the first 100 trees:
```
% trees-consensus newick.trees --skip=100 > c50.tree
```

Skip the first 20% of trees and take every 10th tree thereafter:
```
% trees-consensus newick.trees --skip=20% -x10 > c50.tree
```

Compute the 50% (majority) and 80% consensus trees:
```
% trees-consensus newick.trees --consensus=0.5:c50.tree,0.8:c80.tree
% trees-consensus newick.trees --consensus=0.5,0.8:c80.tree > c50.tree
```

Compute the MAP tree and write a summary of supported partitions:
```
% trees-consensus --map-tree=MAP.tree --report=partitions.txt
```

Compute the MAP tree and write it to the standard output:
```
% trees-consensus --map-tree=- --report=partitions.txt
```


# REPORTING BUGS:
 BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

