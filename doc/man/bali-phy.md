% bali-phy(1)
% Benjamin Redelings
% Feb 2018

# NAME

bali-phy - Bayesian Alignment and Phylogeny estimation using MCMC

# SYNOPSIS

**bali-phy** _sequence-file1_ [_sequence-file2_ ...] [_OPTIONS_]

**bali-phy** help _topic_

# DESCRIPTION

**bali-phy** estimates multiple sequence alignments and evolutionary trees
 from DNA, amino acid, or codon sequences.  BAli-Phy uses MCMC and Bayesian
 methods to estimate evolutionary trees, positive selection, and branch
 lengths while averaging over alternative alignments.

 BAli-Phy can also estimate phylogenies from a fixed alignment (like MrBayes
 and BEAST) using substitution models like GTR+gamma.  BAli-Phy automatically
 estimates relative rates for each gene.

# GENERAL OPTIONS

For each option below, more information is available by specifying the long form of the option as a help topic.  For example: bali-phy help alphabet

**-h**, **--help**, **--help**=_topic_
: Display a friendly help message.  Specify **--help=advanced** or **--help=expert** to display more advanced options.

**-v**, **--version**
: Print version information.

**-t**, **--test**
: Analyze the initial values and exit.

-V, --verbose, --verbose _NUM_
: Print extra output to aid in trouble-shooting.  If _NUM_ is not specified the default is 1.  Values from 2 to 4 increase the amount of information displayed.

-c _filename_, --config _filename_
: Read commands from _filename_ before command line.

# MCMC OPTIONS

**-i** _NUM_, --iterations _NUM_
: The number of iterations to run.

-n _STRING_, --name _STRING_
: Name for the output directory to create.

-x _NUM_, --subsample _NUM_
: Factor by which to subsample.  This option should usually not be used.

-s _NUM_, --seed _NUM_
: Random seed.  Useful for replaying specific runs when trouble-shooting.

# PARAMETER OPTIONS
-T _filename_, --tree _filename_
: File with initial tree in Newick format or NEXUS format.

-U, --unalign
: Unalign all variable-alignment partitions before starting MCMC instead using the supplied alignment as a starting value.

# MODEL OPTIONS
-A _alphabet_, --alphabet _alphabet_
: The alphabet.

-S _model_, --smodel _model_
: The substitution model.

-I _model_, --imodel _model_
: The insertion-deletion model.

-B _prior_, --branch-lengths _prior_
: Prior on branch lengths.

-R _prior_, --scale _prior_
: Prior on the scale.

-L _NUMS_,  --link _NUMS_
: Link partitions.  Takes a comma-separated list of numbers indicating partitions.  For example **--link 1,2,3**.

# EXAMPLES

bali-phy dna.fasta --smodel gtr
: Analyze sequences in _dna.fasta_ under the GTR model.

bali-phy dna.fasta -S gtr -I none
: Perform a traditional fixed-alignment analysis with gaps treated as missing data.

bali-phy dna.fasta amino.fasta codons.fasta -S 1:gtr -S 2:lg08 -S 3:gy94
: Perform an analysis of 3 genes where each gene has a different substitution mode. The sequence names in all three files must be the same.

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

# SEE ALSO

bp-analyze
