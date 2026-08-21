% bali-phy(1)
% Benjamin Redelings
% Aug 2026

# NAME

**bali-phy** - Bayesian Inference of Alignment and Phylogeny

# SYNOPSIS

**bali-phy** [_OPTIONS_] [_INFER-OPTIONS_] _DATA_ ...

**bali-phy** [_OPTIONS_] **run** [_COMPILER-OPTIONS_] _PROGRAM_ [_PROGRAM-ARGUMENTS_ ...]

**bali-phy** [_OPTIONS_] **print** [_PRINT-OPTIONS_] _EXPRESSION_

**bali-phy** [_OPTIONS_] **type** _NAME_

**bali-phy** [_OPTIONS_] **test-module** [_COMPILER-OPTIONS_] _MODULE_

**bali-phy** **help** [_TOPIC_]

# DESCRIPTION

**bali-phy** estimates multiple sequence alignments and evolutionary trees
from DNA, amino acid, or codon sequences. BAli-Phy uses MCMC and Bayesian methods
to estimate evolutionary trees, positive selection, and branch lengths while
averaging over alternative alignments. It can also estimate phylogenies from a
fixed alignment using substitution models such as GTR+gamma.

A named command is optional. Without one, BAli-Phy analyzes the supplied sequence
data. Use **run** for a standalone Haskell program. In a **run** command, the first
non-option is the program. Every later argument belongs to that program, including
arguments beginning with `-` or `/`; no `--` separator is needed.

Long option names must be exact. For example, use **--iterations**, not
**--iter**.

# COMMANDS

**run** _PROGRAM_ [_ARGUMENT_ ...]
: Load a standalone Haskell program and evaluate its `main` function. BAli-Phy
  compiler options must occur before _PROGRAM_. Later arguments are passed to
  the program unchanged.

**print** _EXPRESSION_
: Evaluate and print a model-language expression.

**type** _NAME_
: Print the type of a qualified Haskell name.

**test-module** _MODULE_
: Parse, typecheck, and optimize a Haskell module without running it.

**help** [_TOPIC_]
: Show basic cumulative help or help for a command, option, model, distribution,
  or other semantic topic. **help advanced**, **help expert**, and
  **help developer** reveal successive cumulative levels. The older
  **--help=advanced** form is not accepted.

# GLOBAL OPTIONS

**-h**, **--help**
: Display help for the current command.

**-v**, **--version**
: Print version information.

**-V**, **--verbose**, **--verbose** _NUM_
: Print diagnostic output. If _NUM_ is omitted, use level 1. Values from 2 to 4
  increase the amount of information displayed.

**-s** _NUM_, **--seed** _NUM_
: Set the random seed.

**-P** _PATHS_, **--package-path** _PATHS_
: Add directories to the Haskell package search path. Repeated command-line and
  configuration-file values are combined in that order.

**--set** _KEY_=_VALUE_
: Set a process configuration value.

# INFER OPTIONS

**-t**, **--test**
: Analyze the initial values and exit.

**-c** _FILE_, **--config** _FILE_
: Read inference options and model-language definitions from _FILE_. A scalar
  option on the command line overrides its configuration-file value. Repeated
  options from both sources are combined, with command-line values first.

**-i** _NUM_, **--iterations** _NUM_
: Set the number of MCMC iterations.

**-n** _STRING_, **--name** _STRING_
: Set the base name for the output directory.

**-x** _NUM_, **--subsample** _NUM_
: Set the subsampling factor. This option should usually not be used.

**-l** _FORMAT_, **--log-format** _FORMAT_
: Select `tsv`, `json`, or `tsv,json` scalar logs.

**-T** _PRIOR_, **--tree** _PRIOR_
: Set the tree prior or initial tree.

**-U**, **--unalign**
: Unalign variable-alignment partitions before starting MCMC.

**-A** _ALPHABET_, **--alphabet** _ALPHABET_
: Set the alphabet for one or more partitions.

**-S** _MODEL_, **--smodel** _MODEL_
: Set the substitution model for one or more partitions.

**-I** _MODEL_, **--imodel** _MODEL_
: Set the insertion-deletion model for one or more partitions.

**-R** _PRIOR_, **--scale** _PRIOR_
: Set the prior on the scale for one or more partitions.

**-F** _SPECIFICATION_, **--fix** _SPECIFICATION_
: Fix a topology, tree, or alignment.

**-L** _PARTITIONS_, **--link** _PARTITIONS_
: Link attributes across a comma-separated list of partitions.

**--variables** _SOURCE_
: Add model-language definitions.

# CONFIGURATION FILES

Configuration files are inputs to the default inference mode. An option is written
on its own line as `:option value`. Blank lines and lines whose first non-whitespace
character is `#` are ignored. Every other line is model-language source; these source
lines may appear before, between, or after option lines and retain their relative
order. Sequence data can be supplied with `:align FILE`.

BAli-Phy does not load an implicit `~/.bali-phy` file. Pass a configuration file
explicitly with **--config**.

# HASKELL COMPILER OPTIONS

Compiler options are global. With **run**, place them before _PROGRAM_; with
**test-module**, place them before _MODULE_.

**--cpp**
: Conditionally preprocess every Haskell source module. Without this option,
  preprocessing is enabled only by a leading `{-# LANGUAGE CPP #-}` pragma.

**-D** _MACRO_, **--cpp-define** _MACRO[=TEXT]_
: Define a CPP macro. An omitted replacement defaults to `1`. This option does
  not itself enable CPP.

**--cpp-undefine** _MACRO_
: Remove an initial CPP macro definition.

**--dump-cpp**
: Print Haskell source after conditional preprocessing.

**--dump-ffi**
: Show grouped foreign-import ABI information. This diagnostic requires the
  **test-module** command.

# WINDOWS OPTIONS

Native Windows accepts both the portable Unix spellings above and slash forms
such as `/V`, `/iterations:1000`, and `/test`. Slash forms are not enabled on
Linux, macOS, or WSL2.

# EXAMPLES

`bali-phy dna.fasta --smodel GTR`
: Analyze sequences in _dna.fasta_ under the GTR model.

`bali-phy dna.fasta -S GTR -I none`
: Perform a fixed-alignment analysis with gaps treated as missing data.

`bali-phy --seed 1 run Model.hs --iterations=1000 data.fasta`
: Run a standalone model with a BAli-Phy seed and program-specific arguments.

`bali-phy help advanced`
: Show basic and advanced commands and options.

# REPORTING BUGS

BAli-Phy online help: <http://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

# SEE ALSO

bp-summarize
