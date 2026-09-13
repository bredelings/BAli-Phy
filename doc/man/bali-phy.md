% bali-phy(1)
% Benjamin Redelings
% Aug 2026

# NAME

**bali-phy** - Bayesian inference of alignment and phylogeny.

# SYNOPSIS

**bali-phy** \[OPTIONS\] _SEQUENCE-FILE_ [_SEQUENCE-FILE_ ...]

# DESCRIPTION

**bali-phy** uses MCMC to sample evolutionary trees, alignments, and model parameters from their
posterior distribution. It analyzes nucleotide, amino-acid, and codon sequences. The companion
command **bpy-summarize** produces a report with estimates and convergence diagnostics.

By default, BAli-Phy estimates the alignment as well as the tree; unaligned sequences are suitable
input. To keep the alignment fixed, supply aligned sequences of equal length and use **`-I none`**.
In this case, gaps are treated as missing data.

# EXAMPLES

Estimate alignment and phylogeny:

    bali-phy sequences.fasta

Analyze a fixed alignment:

    bali-phy aligned.fasta -I none

Specify a substitution model with rate variation among sites:

    bali-phy sequences.fasta -S 'GTR +> ASRV.Gamma'

Set the run length and random seed:

    bali-phy sequences.fasta --iterations=10000 --seed=12345

Summarize the resulting run (assuming its directory is **sequences-1**):

    bpy-summarize sequences-1

You can summarize runs while they are still in progress and update the report later.

# GETTING HELP

Use **`bali-phy --help`** for common options. Topic help describes option syntax and model choices:

    bali-phy help smodel
    bali-phy help HKY85

The hyphenated option forms shown here work on all platforms. Native Windows also accepts
slash forms such as `/iterations:1000`.

The user's guide provides tutorials and fuller explanations of models and analyses:
<https://www.bali-phy.org/README.html>.

# INPUT AND OUTPUT

Sequence files may use FASTA or PHYLIP format. Each file supplies a data partition, numbered from
1 in command-line order. Partitions share the tree but may have different models and parameters.

Each analysis creates a new output directory, normally based on the input filename with a numeric
suffix: **sequences-1**, **sequences-2**, and so on. Use **`--name`** to choose another base name.
Principal files within the directory include:

**C1.log**
: Sampled numeric parameters in the default TSV log format; inspect with **statreport**.

**C1.trees**
: Sampled trees.

**C1.P1.fastas**
: Sampled alignments for partition 1, including ancestral sequences, when alignment samples
  are logged. Other partitions have corresponding files.

**bpy-summarize** reads the run directory and writes **Results/index.html** by default.
See the user's guide for the full inventory of output files.

# INFERENCE OPTIONS

## Running an analysis

**`-i`** _NUM_, **`--iterations`** _NUM_
: Number of MCMC iterations. The default is 200000. This specifies run length, not a convergence
  criterion; inspect the results and diagnostics to assess whether more sampling is needed.

**`-s`** _NUM_, **`--seed`** _NUM_
: Set the random seed. If omitted, a seed is generated automatically. The seed is recorded in
  the analysis information. This option also applies to other commands, including **print**.

**`-t`**, **`--test`**
: Initialize the model and evaluate its initial state, then exit without running MCMC.

**`-c`** _FILE_, **`--config`** _FILE_
: Read analysis options and model-language definitions from FILE. See CONFIGURATION FILES.

**`--align`** _FILE_
: Supply a sequence file, as an alternative to a positional filename. May be repeated.

## Models and parameters

The alphabet, substitution model, indel model, and scale options accept partition prefixes.
See MODEL EXPRESSIONS AND PARTITIONS below.

**`-A`** _ALPHABET_, **`--alphabet`** _ALPHABET_
: Set the sequence alphabet, for example `DNA`, `RNA`, `Amino-Acids`, or `Codons`.
  Otherwise, an alphabet is inferred from the input. Specify codons explicitly for codon analyses.

**`-S`** _MODEL_, **`--smodel`** _MODEL_
: Set the substitution model. The default depends on the alphabet. Use **`bali-phy help smodel`**
  for defaults and examples, and topic help for individual models and their parameters.

**`-I`** _MODEL_, **`--imodel`** _MODEL_
: Set the insertion-deletion model. The default is `RS07`. Use `none` to keep the input alignment
  fixed and treat gaps as missing data.

**`-R`** _EXPRESSION_, **`--scale`** _EXPRESSION_
: Set a partition's branch-length scale or its prior. The default is `~Gamma(0.5,2)`;
  a constant such as `1` fixes the scale. Scale defaults to 1 when the tree is fixed.

**`-T`** _PRIOR_, **`--tree`** _PRIOR_
: Set the tree prior. The default is `~UniformTree(taxa)`. Quote the expression, for example
  `--tree '~UniformTree(taxa)'`. Use **`--fix`** to supply a fixed tree or topology.

**`-F`** _TARGET=FILE_, **`--fix`** _TARGET=FILE_
: Use `topology=FILE` to fix the topology while estimating branch lengths, or `tree=FILE` to
  fix both. FILE may contain a Newick or Nexus tree. These forms cannot be combined with
  **`--tree`**. To fix the alignment for MCMC, use **`-I none`**.

**`-L`** _PARTITIONS[:ATTRIBUTES]_, **`--link`** _PARTITIONS[:ATTRIBUTES]_
: Share parameters among comma-separated partitions. Attributes are `smodel`, `imodel`, and
  `scale`; omitting the attribute list links all three. For example, `--link 1,2:smodel,scale`
  links substitution-model parameters and scales. Linked specifications must be compatible.

**`--subst-rates`** _MODEL_
: Set variation in substitution rates among branches: `constant` (the default), `relaxed`,
  or a model expression specifying branch rates.

**`--indel-rates`** _MODEL_
: Set variation in indel rates among branches: `relaxed` (the default), `constant`,
  or a model expression specifying branch rates.

**`--variables`** _SOURCE_
: Supply model-language definitions for use in model expressions. Quote the source as one
  argument, or put definitions in a configuration file. May be repeated.

## Output and messages

**`-n`** _NAME_, **`--name`** _NAME_
: Set the base name for the output directory. A numeric suffix selects an unused directory.

**`-l`** _FORMAT_, **`--log-format`** _FORMAT_
: Select `tsv` (the default for inference), `json`, or `tsv,json` for scalar parameter logs.

**`-V`**, **`--verbose`**, **`--verbose`** _NUM_
: Print diagnostic output. Without NUM, use level 1. Levels 2 through 4 provide more detail.

**`-h`**, **`--help`**
: Display help for the current command.

**`-v`**, **`--version`**
: Print version information.

# MODEL EXPRESSIONS AND PARTITIONS

Quote model expressions containing spaces or shell punctuation:

    bali-phy sequences.fasta -S 'HKY85 +> ASRV.Gamma'

For partition-specific settings, prefix the expression with partition numbers and a colon:

    bali-phy gene1.fasta gene2.fasta -S '1:HKY85' -S '2:GTR'

Without a prefix, **`-S HKY85`** gives each partition a separate copy of the model with separate
parameters. **`-S '1,2:HKY85'`** gives those partitions a shared model and shared parameters.
The same distinction applies to indel models and scales. Alphabet prefixes choose which
partitions receive an alphabet. Repeat an option for different partitions; do not specify the
same partition twice for the same attribute.

## Inspecting an expression with print

**bali-phy print** _EXPRESSION_ [**`-A`** _ALPHABET_]

Evaluate a model-language expression and display its value. This is useful for exploring models
without running an analysis. Supply an alphabet when the expression depends on one:

    bali-phy print HKY85 -A DNA
    bali-phy print 'HKY85 +> ASRV.Gamma' -A DNA

Model parameters with priors are sampled when the expression is evaluated. Use **`--seed`** to
repeat a particular draw. For these examples, the output is a rate matrix or a mixture of rate matrices.

# CONFIGURATION FILES

For example, **analysis.config** could contain:

    :align sequences.fasta
    :smodel GTR +> ASRV.Gamma
    :iterations 10000
    :name analysis

Run it with:

    bali-phy --config analysis.config

Write each option as `:option value`, using its long name. Model expressions in the file do not
need shell quotes. Blank lines and lines beginning with `#` after optional whitespace are ignored.
Other lines supply model-language definitions and retain their order.

A scalar command-line option overrides its configuration-file value. Repeated options, such as
sequence files and model specifications, are combined, with command-line values first; avoid
assigning a model twice to the same partition.

# ADVANCED COMMANDS

These commands support standalone programming and compiler inspection. For additional help,
**`bali-phy help advanced`**, **`bali-phy help expert`**, and **`bali-phy help developer`** show
successively more detailed command-line help.

**bali-phy run** _PROGRAM_ [_ARGUMENT_ ...]
: Load a standalone Haskell program and evaluate its `main` function. Put BAli-Phy options
  before PROGRAM. All later arguments belong to the program, including arguments beginning
  with a hyphen; no separator is needed. For example:

      bali-phy --seed=1 run Model.hs --iterations=1000 data.fasta

**bali-phy type** _NAME_
: Print the type of a qualified Haskell name.

**bali-phy test-module** _MODULE_
: Parse, typecheck, and optimize a Haskell module without executing it.

**`-P`** _PATHS_, **`--package-path`** _PATHS_
: Add directories to the Haskell package search path. May be repeated.

**`--set`** _KEY=VALUE_
: Set an internal process configuration value.

# DEVELOPER OPTIONS

These options are for compiler debugging and tuning, and are unnecessary for ordinary analyses.
They apply globally. With **run**, place them before PROGRAM; with **test-module**, before MODULE.

**`--dump-parsed`**, **`--dump-rn`**, **`--dump-tc`**, **`--dump-ds`**, **`--dump-opt`**
: Show compiler output after parsing, renaming, typechecking, desugaring, or optimization,
  respectively.

**`--dump-ffi`**
: Show grouped foreign-import ABI information. Requires **test-module**.

**`--optimize=false`**
: Disable the optimizer. Optimization is enabled by default.

**`--inline-threshold`** _NUM_
: Set the inliner's size threshold (default 8). Larger values permit more inlining.

**`--cpp`**
: Enable conditional preprocessing for every Haskell source module. Otherwise, preprocessing
  is enabled by a leading `{-# LANGUAGE CPP #-}` pragma in a module.

**`-D`** _MACRO[=TEXT]_, **`--cpp-define`** _MACRO[=TEXT]_
: Define a CPP macro, with replacement `1` if TEXT is omitted. Does not itself enable CPP.

**`--cpp-undefine`** _MACRO_
: Remove an initial CPP macro definition.

**`--dump-cpp`**
: Show Haskell source after conditional preprocessing.

# REPORTING BUGS

BAli-Phy online help: <https://www.bali-phy.org/docs.php>.

Please send bug reports to <bali-phy-users@googlegroups.com>.

# SEE ALSO

bpy-summarize(1), statreport(1), bali-phy-pkg(1)
