# 4.2 (unreleased)

## Features
- Log departure from reversibility in terms of the flux and nonreversibility.

## Fixes
- Fix non-reversible models + Markov-modulated models.

## Speed
- Improve startup speed.
- Speed up likelihood calculations for models with lots of states (e.g. **4-fold speedup for codon models**).

## Internal
- Refactor optimizer.
- Improve specialization of polymorphic functions for particular types.

## Documentation
- Add documentation for nonRev and nonEq CTMC models.

# 4.1 (Jun 9, 2025)

## Features
- Show multiple different consensus alignments in bp-analyze.

## Speed
- Don't spend so much time on SPR for large trees.

## Fixes
- Fix alignment-consensus --strict=1
- Fix character ranges for 1,2,3rd codon position.
- Fix bp-analyze for Numeric(2) alphabet.
- Fix default alphabet and smodel for binary data.
- Fix allow scaling move to work on negative values.
- Fix proposed alignments in SPR.

## Unstable features
- Allow conditioning on "variable" sites.
- Make SPR move work on time-trees.
- Speed up coalescent tree probability.

## Internal
- Refactor let-floating optimization ot use Core2 AST.

# 4.0 (Feb 27, 2025)
- Models
  - Allow indel rate variation, relative to the substitution rate.
- Build:
  - Fix homebrew build on MacOS 13.
- Misc:
  - Print priors before long pause if compiling haskell modules for the first time.
  - Allow printing branch attributes on Newick trees.

# 4.0-beta17 (Jan 22, 2025)
- Models:
  - Multi-Nucleotide-Mutation (MNM) model for codons.
  - BUSTED and BUSTED-S substitution models for positive selection.
  - Initial cmdline version of `multi_freq` model.
- Bug fixes:
  - Log sorted (conserved) omegas in m3_test.
- Speed:
  - Use sparse likelihood vectors for leaf branches.
  - Use faster eigenvalue method to compute exp(Q*t) by default, falling back to slower method if needed.
- Haskell:
  - Implement strict fields for data types with !type.
  - Improved error messages for type errors.

# 4.0-beta16 (Nov 23, 2024)
- Speed: further speed up tree operations.
- Output: simplify results from ancestral sequence reconstruction.
- Input: allow defining models using multiple lines.
- Models:
  - Speed up and improve RNA editing model.
  - Refactor coalescent tree distribution.
  - Add Yule tree distribution.
  - Allow '--tree=~uniform_tree(taxa,gamma(0.5,2/length(taxa)))'
  - Allow trees to be labeled with things besides Text.
- MCMC:
  - Allow time-tree-NNI to interchange around the root.
  - Allow time-tree-NNI to resample alignments.
  - Use multiple transition kernels for uniform_int distribution.
  - Allow hand-written Metropolis-Hastings proposals for numbers/atomic objects.
- Bug fixes:
  - Fix crashes in module writing/reading.
- Tests:
  - Refactor tests so that `ninja test` runs each testsuite test separately.
  - Add CI for `ninja test`.
- Haskell:
  - Make [1 ..] and [1.5 .. ] work for floating point.
  - Make ['a'..'j'] work for characters.
  - Add function `display` that converts things to Text.
  - Fill out Data.Semigroup and Data.Monoid
  - Handle annotations of overlapping instances.
- Internal:
  - Rewrite the simplifier optimization pass to use the Core2 AST representation.

# 4.0-beta15 (Jul 25, 2024)
- Speed increases:
  - Speed up tree operations.
  - Don't scan the whole tree to do SPR.
- Models:
  - Add relaxed-rs07 that allows rate to vary on different branches.
- Infrastructure:
  - Non-equilibrium models now work with variable alignments.
- Bug fixes:
  - Fix inferring the alignment on a fixed topology (rooted or unrooted).
  - Invalidate compiled modules if the executable changes.
  - Fix consensus-tree and branch lengths ignoring burnin when not specified as percent.
  - Fix (unused) code that integrates out alignments when SPR-attaching on every branch.
- Syntax
  - Cmd-line functions now look like |w: gy94(omega=w)|
  - Cmd-line functions can now take two arguments like |x y: x + y|

# 4.0-beta14 (Jun 20, 2024)
- Features
  - Allow inferring the number of bins under the Rates.free and M3 models.
  - Make startup faster by saving compiled modules.
  - Sort rates under Rates.free to make them identifiable.
  - Sort omegas under M3 to make them identifiable.
  - Allow specifying a discrete distribution as a map of {value: prob, value: prob, etc.}
  - New distribution `dirichlet_mixture` to make variable-dimension mixtures easier.
- Bug fixes:
  - Infer the character state for N in leaf sequences with a fixed alignment.
  - bp-analyze should not fail when variables have random dimension.
- Refactoring
  - Kind is now an alias to Type (from coretype.H) instead of expression_ref.
  - CompiledModule is now separate from Module.  Programs contain only CompiledModule.

# 4.0-beta13 (May 25, 2024)
- Bug Fixes:
  - Fix typo in the bp-analyze script!
  - Fix crash in statreport.
  - Fix crash in draw-tree.
  - Fix crash in trees-bootstrap.

# 4.0-beta11 (May 22, 2024)
- Features
  - Improve alignment-thin --remove-gappy
- Misc
  - JSON: read/write {Infinity, NaN} instead of {1e99999, null}
  - Make +> work like the R pipe operator.
- Bug fixes:
  - Correctly compile with libfmt v10.
  - Fix warning in bp-analyze.
  - Only write warning in Rates.gamma at -V2 or higher.
  - Complain instead of crashing when we use an imodel with --fix=alignment.

# 4.0-beta10 (May 17, 2024)
- Features
  - Allow codon models with any genetic code.
  - Improve help output -- colorize, more readable types, etc.
  - Allow equilibrium non-reversible models with variable alignments.
- Bindings
  - Move calls and default values to the expression parser.
  - Use the parser to read types from bindings files.
  - Rewrite: change types from Distribution[Double] to Distribution<Double>
  - Rewrite: use a -> b for Function<a,b> and (a,b) for Tuple<a,b>.
- Bug fixes:
  - Fix non-standard genetic codes.
  - Fix specifying Codons, Codons(DNA), and Codons(DNA,mt-vert) alphabets.
  - Fix density for Exp-transformed distributions like log-normal.
- Misc:
  - Allow `bali-phy <command> arguments` for commands print, help, run, and model.
  - Help: show argument types instead of default values.
  - Allow floating point numbers to be any type.
  - Refactor Numeric types Log Double and Prob.
  - Add class RealFloat with checking for NaN and Inf.

# 4.0-beta9 (Mar 30, 2024)
- Bug fixes:
  - Fix crash with long sequences.
  - Fix crash in meson test
  - Fix numeric alphabet (e.g. 0/1 characters)
  - Fix bp-analyze on tree-only runs.
  - Fix quotes and _ in Nexus names.
  - Fix singularity build.
- Non-reversible substitution models
  - Currently only enabled for fixed-alignments. Will work for variable alignments in the next release.
  - Generate rooted tree for non-reversible substitution model.
  - Log computed frequencies for non-reversible substitution model.
  - Add model nonreversible
- Haskell
  - Allow parsing and renaming data families and instances.
  - Simplify the seqs in the IO monad.
- Alignment
  - Allow sample-alignment on multifurcating trees.
- Machine
  - Remove constant-with-force in favor of index-var-with-force to constant.
  - Remove forced_by edges.
  - Remove index-var edges.
  - Remove index_var_with_force_to_* ==> index_var_with_force.
  - Make index-vars point to final value.
  - Add force edge from index-var to target.
  - Look through a single-force index-var when forcing.
  - Make interchangeables unforgettable.
  - Move called_by and used_by edges to changeable nodes.
- Command-line language        
  - Allow "var ~ dist ; ... "
- Build
  - Add Mac/ARM.

# 4.0-beta8 (Dec 19, 2023)
- Usability tweaks
  - Write out tree with branch-lengths scaled according to weighted average of partition scales.
  - Print the alignment length including fixed-alignment partitions.
- Phylogenetics
  - Update likelihoods and other infrastructure to handle multifurcating trees.
  - Update likelihoods and other infrastructure to handle data on internal nodes.
  - Compute CLVs for observed sequences.
  - Allow tips with no observed sequences.
  - Stop special-casing 1-node and 2-node trees.
- Tools
  - Add benchmarking script benchmark.py
- Fixes
  - Don't allocate new memory in matrix::operator=( ) if we don't have to.
  - Test letter_mask, not letter fmask!
  - Correctly handle ASR for 1-node and 2-node trees.
  - Fix matrix transpose.
- Testing
  - Re-enable the sanitizer test.

# 4.0-beta7 (Nov 1, 2023)
- Bug fixes:
  - Allow estimating the alignment with a fixed topology.
    - Don't crash trying NNI on fixed tree.
    - Allow discovering a single partition by supplying alignment regs.
    - realign_from_tips: don't modify the branch lengths if they are fixed.
  - Fix -l tsv with -m Model.hs
  - Fix default instances for type families.
- Haskell
  - Rename ctmc_on_tree and ctmc_on_tree_fixed_a -> phyloCTMC.
  - Rename random_alignment -> phyloAlignment.
  - Can now simulate from phyloCTMC.
  - Can now simulate data on fixed alignment with no gaps.
  - Ancestral sequences are now AlignedCharacterData.
  - Sampled/observed sequences are Unaligned/AlignedCharacterData depending on if alignment is fixed.
  - Common methods between AlignmentOnTree and AlignedCharacterData now in class Alignment.
  - Refactor generic methods in Tree class into classes Graph and Forest.
  - AlignmentOnTree: Only store sequence lengths for some nodes.
  - Replace Sequence with (,)
  - Stop using C++ alignment class when compressing sequence data.
  - Add some instances for Data.Map
- Machine:
  - Better error message if trying to read/modify a non-modifiable.
  - Add associated type DistProperties, and return it from observed.
  - Add sampleWithProps -- hacky version.
  - Synchronize property indices between between variable and fixed-A.
  - Remove unused property prop_taxa.

# 4.0-beta6 (Sep 6, 2023)
- Changes:
  - MAP alignment is no longer printed.
  - the MPD alignment is now shorter and more condensed (it uses the "multiply" criterion).
  - the initial alignment has all sequences left-aligned when estimated (for now).
  - "-m Model.hs" now looks for a Model.main function that returns IO (Random [(Key,JSON)]).
  - stdout / stderr are no longer redirected to a file.
- Features:
  - Tree-alignment models can be hand-written -- no magic return type.
  - Initial implemention of sequence simulation.
  - new executable mcon-tool can convert JSON logs to TSV.
- Bug fixes:
  - Memory: reclaim memory from running loggers + transition kernels.
  - Memory: clear tokens immediately - don't wait for garbage collection.
  - Memory: don't retain calculations for branches that might not exist.
  - Alignment: fix alignment with likelihood rescaling.
  - Likelihood: fix SEV likelihood on 2-taxon trees.
  - Typechecking: don't allow e.g (x,y) <- return (x,y,z)
  - Typechecking: handle exporting types without their constructors.
  - machine: don't re-execute side-effects in incremental_evaluate1( ).
- Model language:
  - Allow reading / writing trees and alignment from the model.
  - Logging is done from Haskell - remove C++ logging.
  - Transition kernels are run Haskell
  - sample branch-lengths from an iid IntMap of distributions.
  - Stop setting alignments from the Parameters constructor.
  - Refactor Tree
    - the degree of a node can change dynamically.
    - node and branch names don't need to be in [0..n-1].
    - branch names are reversed by negation.
    - simplify modifying the topology in TreeInterface.
- Haskell
  - Reimplement Data.Text more efficiently.
- Debugging
  - Improve the clarify of the printed execution graph.

# 4.0-beta5 (Jun 27, 2023)
- Bug fixes:
  - Fix problem with Covarion models (affected RNA-editting model).
  - Don't treat JSON integers as if they were floating point.
- New Singularity executable to run on clusters with old software.
- Compute values for MCMC logging in the model language:
  - Warning: Temporarily remove pre-burnin.
  - Warning: If --tree is specified then scales default to a constant 1.0 so that the likelihood tests work
  - Print trees and alignments efficiently from Haskell.
  - Compute parsimony score in Haskell.
- Move MCMC move setup into the model language:
  - Move slice-sampling and MH moves from C++ into Haskell.
  - Stop running C++ MCMC moves -- just run Haskell moves.
- Implement Haskell functionality
  - Implement Haskell File IO.
  - Add Data.IORef for mutable values.
  - Add Data.Unique and Data.Unique.Id.
  - Make it a lot easier to define new IO operations.
- Cleanups
  - Remove eigen sources from git, automatically download if not installed.
  - Stop exporting the scale from Haskell to C++.


# 4.0-beta4 (May 12, 2023)
- Bug fixes:
  - compile issue on Macs.
  - fix R script bug.
  - gamma / exponential variables weren't updating in MCMC
- Add strict consensus to alignment-consensus
- Mention the CPU architecture at the end of meson setup.
- Haskell;
  - properly complain about overlapping instances.
  - properly export imported constructors.


# 4.0-beta3 (Apr 21, 2023)
- Changes
  - Command-line model language
    - Change model-stacking operator from '+' to '+>'
    - Add numeric '+', '-', '*', '/'
    - Functions are now called like f(x) instead of f[x].
    - Lambda functions are now written `function(w: E)`.
    - Allow specifying lists with [x,y], tuples with (x,y).
    - Allow specifying dictionaries (lists of pairs) like {"A":x,"B",y}.
    - Allow writing 'x=v; E' for 'let[x=v,E]'.
    - Don't allow 'f(,2)' anymore.
  - Improve MCMC mixing when inferring number of clusters.
  - New numeric type Prob that handles numbers very close to 1 by storing the log-odds.
  - Improve handling of numeric types.
  - Refactor how probability distribution work in Haskell.
  - Bug fixes for the Haskell typechecker.

# 4.0-beta2 (Mar 21, 2023)
- Features
  - Allow sequences names to contain Newick special characters, but give warning.
  - Update command-line model syntax to be more readable.
- Haskell
  - Optimization: eliminate unused error paths.
  - Decrease memory usage from compilation.

# 4.0-beta1 (Mar 9, 2023)
- Bugs
  - FIX likelihoods for fixed alignments (introduced in 4.0alpha-10).
  - Run automated tests for fixed alignments too.
  - FIX sample_ancestral_sequences.
- Better error messages for type errors.

# 4.0-alpha10 (Feb 21, 2023)
- Bugs
  - FIX using too much memory for large trees.
  - FIX JSON printing empty arrays and objects.
- Haskell
  - Fix imports/exports of types & associated values.
  - Default to Integer instead of Int.
  - Correctly use == for literal patterns.
  - Handle Language options better.
  - Make list ops work on Arrays and other Foldable things.
  - Better error messages
    - Convert exceptions to error messages.
    - Add locations to more error messages.
    - Report context (givens).
    - Improve readability.
    - Improve error messages on superclass constraints.
    - Readable error messages for parser.
  - Speed
    - Unify eagerly, don't create empty implications.
    - Implement case-merging optimization.
    - Implement (seq x E) as (case x of _ -> E).
- Refactor Tree data structure
  - More efficient leaf labels.
  - Convert out-edges to Array.
  - Lookup nodes/edges in an IntMap.
- Alignments
  - Stop reordering alignments -- just look up sequences by name.
- Virtual machine
  - Don't forget modifiable fields of data structures even when unused.
  - Don't force fields in alignments and trees -- saves RAM.
  - Look through some constants when forcing.
  - Simplify context_ptr to inspect the VM from C++.
- MCMC
  - Allow using random operations in MCMC proposals.


# 4.0-alpha9 (Jan 9, 2023)
 - Models
   - Add Covarion.gtr to allow more flexible rates between classes.
   - Add the `iid_on object sub_distribution` distribution.
 - Fixes
   - Fix tree MDS plots
   - Don't segfault when the branch-length isn't modifiable.
 - Tooling
   - remove internal copy of boost
 - Haskell infrastructure
   - Add type families / associated types
   - Add some readable error messages that show the error location, with color.
   - Add unbounded Integer type, in addition to Int

# 4.0-alpha8 (Nov 15, 2022)
 - Further improvements to the Haskell type system -- allow GADTs and equality constraints.
 - Fixes to trees-bootstrap.
 - Accept '?' again as maybe-an-N-maybe-a-gap.

# 4.0-alpha7 (Aug 18, 2022)
 - make BES work with typechecking
 - allow specifying model arguments as `-m Model arg1 arg2`
 - alignment-consensus and alignment-gild can take a list of alignment files
 - use std::filesystem for file operations.
 - target Mac release builds to OS X 10.15

# 4.0-alpha6 (Jul 17, 2022)
 - Haskell typechecking is working!

# 4.0-alpha5 (Apr 29, 2022)
 - Models
   - RNA editting model: new
   - Li & Stephens model: specify snp columsn and locations instead of the whole alignment matrix.
 - Math
   - Switch to slower-but-more-accurate matrix exponential code that handles
     the case when equilibrium frequencies are 0.
 - Haskell infrastructure
   - More parser / AST cleanup.
   - More renamer cleanup.
   - Perform dependency analysis on the value declarations.
   - Preliminary typechecking pass.
     - handle let, case, lambda, etc.
     - accumulate constraints
     - simplify constraints
     - handle fromInteger, fromRational
     - implement monomorphism restriction
     - implement defaulting
     - kind-check and quantify explicit signatures
     - handle exp :: type
     - split recursive groups by signature
 - Tools
   - alignment-max
     - Allow columns to be distinguished based on the number of letters emitted before a gap.
   - alignment-smc
     - Allow selectings sites by fraction of sequences missing at that site.

# 4.0-alpha4 (Dec 20, 2021)
 - Fix alignment_slice_sample_branch_length
 - Type system
   - Implement kind inference
   - Create Haskell AST structs
   - Remove struct AST_node
   - Clean up the renamer pass.
   - Refactor Module::compile( )
 - Refactor log_double_t
 - Add builtins for expm1 and logp1
 - Fix mixture distribution
 - Make test take input file as a command line argument.
 - Refactor the simplier to work better.
 - Make generated Haskell code less verbose.

# 4.0-alpha3 (Aug 21, 2021)
 - Create a dynamic factor graph
 - Annotate random variables in the factor graph with "properties".
 - Allow constructing data_partition objects from factor graph properties.
 - Allow constructing Parameters objects from a tree reg.
 - Port C++ transition kernels to Haskell models.
   - Tree and alignment estimation now actually work.
 - Optimization: ignore zero-rate transition kernels.
 - Logging:
   - Allow logging the |T| and |A|
 - Change rs07 function to take the indel rate, instead of the log.

# 4.0-alpha2 (Jul 31, 2021)
 - Preliminary relaxed-clock trees.
 - Simplify the Haskell modelling language -- use the strict monad.
 - Create a dynamic factor graph and record properties on it.
 - Simplify BAli-Phy.Main.hs generated from the command-line.
 - Fixes:
   - fix --print.
   - fix and update documentation for m8, m8a, and m8a_test.
 - Type system:
   - sort declarations into groups, in preparation for kind inference / kind checking.
   - parse and load type classes and instances.
 - Parser:
   - create AST objects instead of treating everything as an s-expression
   - print line numbers for undeclared variables.

# 4.0-alpha1 (May 6, 2021)
 - Lazy random variable creation and destruction.
 - MCMC
   - Switch to slice sampling by "doubling", instead of "stepping out".
 - Probabilistic programming: models
   - Rooted trees
     - Allow sampling time trees.
     - Add MCMC moves to estimate node times.
   - Add NNI moves for unrooted trees.
   - Add Data.CSV and Data.Frame modules to load data from CSV files.
   - New distributions
     - half_cauchy
     - negative_binomial
     - poisson_process
     - multinomial
     - sample_markov
   - MCMC
     - Allow rates of moves to change dynamically.
     - Allow moves to appear or disappear during an iteration.
     - Add moves when variables are created for more distributions.
 - Probabilities
   - Eliminate prior_no_alignment(), OS (other_subst) and OP (other_prior).
     Instead, compute the ratio of summing out the alignment to not summing it out.
 - Probabilistic programming: machinery
   - Eliminate the closure stack.
   - Add force edges, and allow forces from constants and index_var's.
     - index_var's can have results, but not steps.
     - Add index_var edges, which are fixed call edges.
   - Combine unsharing and invalidation when we evaluate the program.
   - Program evaluation tokens must be immediately adjacent.
   - Avoid recomputing steps if inputs have the same memory location.
     - Allow sharing steps in this case.
   - Evaluate program
     - Evaluate invalid regs that are unconditionally evaluated.
     - Decrement force counts for calls of invalid steps.
     - Evaluate conditional invalid regs with positive force counts.
   - Compute probability ratios by tracking registration events
     for prior and likelihood terms.

* 3.6.0 (Feb 5, 2021)
 - Make the `"nested": true` form of JSON log format more readable.
 - Connect 3D MDS points with lines.
 - Speed up fixed-alignment analyses.
 - Sample ancestral states for fixed alignments with column compression.
 - Update command-line model language
   - Replace getAlphabet builtin with get_state[alphabet]
   - Add get_state[branch_categories]
   - Add Tuple[...] expression.
   - Stop putting argument names into scope as identifiers.
   - Allow default arguments to depend on earlier arguments.
   - Generate cleaner Haskell code:
     - Don't prefix variable names with "arg_", "let_var_", or "lambda_var_"
     - Rewrite code generation:
       - Track emitted Haskell variable names.
     - Generate readable code for lists and tuples.
     - Handle functions in a cleaner way.
     - Eliminate %>=% in favor of just %=% and %>%.
 - Make modifiable structures instantiate either all their fields or none.
 - Fixes
   - fix crash in alignment-distances NxN
   - decrease time and memory for summarize-alignments

* 3.5 (Mar 2, 2020)
 - Reconstruct ancestral sequences by default.
 - Graphical model framework
     - Brownian model on *random* rooted tree.
 - Misc
     - Switch to c++17
     - Read tree properties that look like [&key=value,&key=value]
 - Models
     - Add +mut_sel_aa for selection on amino-acids in codon models.
     - Add 4 Covarion models: ts98, huelsenbeck02, galtier01, wssr07
 - MCMC
     - Add move to slice sample a branch length while integrating out the alignment.
 - Speed
     - bp-analyze should be faster.
     - Fix slow startup with lots of partitions.
     - Switch contexts less in NNI.
     - Don't waste time memsetting the state array.
 - Graphical models
     - Probability language/interpreter
         - Simplify logging commands:
             - Remove "log_all"
             - Replace %% with %=%, %>% and %=>%.
         - Remove "sample" keyword.
         - Allow sampling characters on random trees (recursive, via mfix).
             - Implement syntactic sugar 'rec' for mfix.
         - Evaluate the program changeably.
         - Switch the interpreter to a lazy interpreter.
         - Separate random variables from modifiables.
             - Make modifiables evaluate to an initial value.
             - Allow specifying modifiable *structures*, such as a list of modifiables.
         - Allow sampling a random tree topology.
         - Observe likelihood of sequences on tree.
         - Sample the tree inside the machine.
         - Observe lists in an efficient manner.
     - bali-phy program
         - Generate a Haskell program that is moderately readable.
         - Make a unified program that computes an ATModel object.
         - Sample ancestral sequences inside program.
     - MCMC
         - Run all dynamically registered transition kernels - not quite right.
         - Make slice sampling bail when number of variables changes.
     - Logging
         - Do logging by computing a JSON object.
         - Allow logging strings.
         - Allow logging all fields in JSON format as well as TSV format.
         - Allow reading tables from JSON formatted output of constant shape.
         - Add TableReader class that uses only one line at a time.
     - Virtual machine
         - Forward-only sharing.
         - unshare_regs: only unshare created-regs with steps.
         - Remove COMBINE_STEPS
         - Fixed uses/forces for each step.
         - Remove reapply.
         - Remove `Result` objects in favor of just an index.
         - Implement side-effects.
         - Record force edges.
         - Garbage-collection no longer deletes steps or results.
         - SPEED: inline mapping routines
         - FIX graphviz description of graph by using HTML records.
     - Haskell syntax
         - Add record syntax for declaring structures.
     - Haskell modules
         - New Data.Eq module.
         - New Data.Text module.
         - New Data.Set module.
         - New Data.Matrix module.
         - Move distributions to Probability.Distribution.NAME modules.
     - Haskell optimization
         - Reimplement "full laziness" via floating out lets and MFEs.
 - Docs
     - Split up developer docs.
 - Help
     - Display functions more compactly with `help functions`
     - Handle `quote` better in terminal output.
     - Allow getting help on functions, models, and distributions.
 - Tools
     - Allow reading from files wherever we take a comma-separated-list
     - alignment-info: add option to show sequence names and/or lengths.
     - extract-ancestors: new program to get specific ancestral sequences.
     - trees-consensus: always write out PP.
     - tree-tool: add command to remove internal node names.
 - Code
     - Refactoring
         - Move utility code under util/
         - Split util/util.H into several other files.
         - Make separate libutil and libcomputation
     - Rewrite bp-analyze in python.
     - Generalize MCMC proposals using std::function.
 - Packaging
     - Distribute `draw-tree` binary on windows.
 - Fixes
     - Correctly escape newick names when writing tree samples.
     - Correctly handle FAIL when desugaring case expressions.
     - Make bp-analyze handle windows line endings when run from cygwin/UNIX.
     - Compile without errors in C++17 mode.
     - Link to math library if available: fix build on kfreebsd
     - Avoid crash with NaN on armhf.
     - Don't print out newick trees rooted at a leaf.
     - Speed up tree drawing in bp-analyze.
     - Correctly compute PP version of MAP and greedy trees.
     - Fix crp / dirichlet process prior.
     - Fix export of entities imported from another module.
     - Stabilize matrix exponential.

* 3.4.1 (Jan 18, 2019)
 - Help
   - Display functions more compactly with `help functions`
 - Packaging
   - Distribute `draw-tree` binary on windows.
 - Fixes
   - Correctly escape newick names when writing tree samples.
   - Correctly handle FAIL when desugaring case expressions.
   - Make bp-analyze handle windows line endings when run from cygwin/UNIX.
   - Compile without errors in C++17 mode.

* 3.4 (Dec 13, 2018)
 - Evolutionary models
   - Add Doublet alphabet for RNA stems.
   - Add generic +mut_sel model modifier.
   - Add models x2, x2_sym, x2x2 for RNA stems.
   - Add RNA.m16a model for RNA stems.
 - Fixes
   - "--" should not become a single long dash (en-dash) in man pages.
   - Don't crash if --scale is set to a constant (e.g. --scale=1).
   - Allow reading (a,b):1.0; by ignoring the root branch length.
   - Properly translate newick labels with quotes or _.
   - Don't replace W with A in observed sequences unless --set infer-ambiguous-observed=true
 - Misc
   - Correctly log things inside a let binding.
   - Allow selecting character ranges from a file: "sequences.fasta:100-240,300-900"
   - Rename subsample to bali-subsample (to avoid conflicts with other software).
   - Install bali-phy-pkg.
   - Make BES package work again.
   - Reorganize fields on C1.log
   - Hold Numeric[k] or Doublets[DNA/RNA] alignments fixed.
   - Allow writing alignments every iteration.
   - tree-tool: add scaling, pruning, computing diameter, etc.
   - cut-range: allow selection samples from more than 1 alignment file
   - alignment-thin: clean up options and man page.
   - alignment-distances: new tool, add accuracy and recall metrics.
 - MCMC
   - Only compute probability *ratios*
     - Allow recovery from initial -infinity.
   - Improved mixing for [0,1] random variables.
 - Graphical model framework
   - Interpreted models now separate the likelihood from the prior in logged output.
   - Interpreted models (e.g. LinearRegression.hs) are now a lot faster.
   - Simplify constructing loggers.
   - Allow using poisson distribution.
 - Haskell
   - Allow "import modid ( .... )" and "import modid hiding ( .... )"
   - Add Data.JSON module
   - Implement quot, rem, div, mod.
   - Implement -X NoImplicitPrelude
   - Refactored functions out of Prelude into Data.List, etc.
   - Implement layout-sensitive parsing.
   - Implement pattern bindings (i.e. let (x,y) = E1 in E2)
   - Implement @-patterns (i.e. x@(y,z) )
   - Implement lazy patterns (i.e. ~(y,z)  )
   - Implement guards for functions and case.
   - Allow modules with no "module Name where" clause.
   - Implement running a module with --run-module
   - Encode strings as (listFromString String)
   - Add flags for dumping parsed, renamed, desugared, etc. code.
   - [FIX] float let out of let if it reveals a constant.

* 3.3 (Aug 6, 2018)
  - Fixes
    - make bp-analyze able to parse output files from 3.0-beta.
    - rename Goldman & Yang codon model to gy94 from incorrect yn94.
    - fix run file for f81 model.
    - fix incorrect LG model.
    - normalize WAG and LG standard frequencies.
  - Codon models
    - gy94 and mg94 now have no submodels and are rate matrices.
    - f1x4, f3x4, and f61 now compute named codon frequencies.
    - add gy94_ext and mg94_ext for using any nucleotide rate matrix.
    - add mg94k for mg94_ext[hky85]
  - Triplet models
    - fix up x3, x3_sym, x3x3
    - add +dNdS function so we can do e.g. hky85+x3+dNdS
  - Functions in models
    - Add new syntax function[x,...] for specifying models.
    - new functions map, zip, zipWith.
    - m1a,m2a,..,m8a_test,branch-site, etc. now take functions as arguments.
    - let-bind names for lambda-dependent arguments, but inside the lambda.
  - Model framework
    - +fe as synonym for +f[Frequencies.uniform]
    - +f and +gwF now take exchange models like wag as a submodel.
    - logging now returns an object that can change shape dynamically.
    - function calls can now look like f[g[x],y] instead of just f[x,y]
    - arguments now referenced with @arg to fix function[x,add[x,x]]
    - fixes to unification and constraints.
    - suppress gamma:shift and exponential:shift
  - Haskell
    - modules can now re-export imported modules.
    - refactor module SModel into SModel.Codons, SModel.ReversibleMarkov, etc.
    - remove hky85', etc now that models can call hky85 directly.
    - add lazy IO
    - make sampling in the Random monad lazy
    - correctly set rates in sample'
    - do normal 0 1 => do sample $ normal 0 1
  - Docs
    - Reorganize docs on substitution models.
    - Move docs on partitions to new section.
  - Etc
    - Allow specifying alphabet for --print.
    - Allow running gy94 from --print.
  - Testing
    - Find out why different package disagree on wag+f[wag_freq]

* 3.2 (Jun 25, 2018)
  - Fixes
    - Increase test timeout for internal testsuite and testiphy.
    - HTML report: don't hide header behind top-bar in Chrome.
  - alignment-smc improvements.
  - change alignment-diff back to red
  - Add NEWS file.

* 3.1.5 (Jun 13, 2018)
  - Fixes
    - Make all programs use shipped libstdc++
    - Make MDS plots handle bp-analyze --subsample
    - Increase test timeouts
    - Correctly write initial alignment for fixed alignment partitions.
    - Don't write "file:" for MDS URLs in HTML report
  - Help
    - Print citations with pmid and pmcid in help.
    - Print help for 0-argument functions like 'dna'
    - Improve help for fMutSel and fMutSel0
  - Add Frequencies.uniform[] function.
  - Make SEV handle site-compression.
  - HTML report: print version number - lots of cosmetic improvements.
  - Add new tool tree-tool (and map page, etc.)
  - Change alignments-diff highlight color back to red.
  - Clean up DP matrix code.

* 3.1.4 (Jun 9, 2018)
  - Fix prior on alpha in Rates.gamma

* 3.1.3 (Jun 9, 2018)
  - Fix mean_length prior in RS05 model.
  - Add some more color-schemes for drawing alignments-diff output.
  - Add ferns exon/intro data set.

* 3.1.2 (May 6, 2018)
  - Fix a testsuite bug
  - Haskell
    - parse exports list

* 3.1.1 (May 5, 2018
  - Fixes
    - Don't require testiphy
  - Print priors on models in their own section.

* 3.1 (May 2, 2018)
  - Fixes
    - Build alignment-thin and alignments-diff
  - Show model and priors readably
  - bali-phy accepted into Debian
  - Change 'logp' => 'posterior'

* 3.0.2 (Mar 10, 2018)
  - Docs
    - man pages for bali-phy and tools
    - man pages online

* 3.0.1 (Mar 6, 2018)
  - Fixes
    - Fix crash in alignment-thin

* 3.0 (Feb 12, 2018)
  - Models
    - Mixture models now work.
    - MultiRate[model,dist,n_bins] also works
    - let[var=E1,E2] now works
    - Rename RA[a] to RevCTMC[a]
    - Stop representing integer, double, and bool as strings.
  - Report
    - subsample trees when running trees-distances for MDS plot
  - Docs
    - Add help for more functions.
  - MCMC
    - Implement SPR+A
    - Destroy SPR_by_NNI with fire.

* 3.0-beta6 (Jan 5, 2018)
  - Fixes
    - Make sorting of DP:rates and M3:omegas work again.
    - sample branch lengths from prior -- fixes very long initial tree.
  - specify branch lengths as a List[Double]
  - Models
    - Make lists into a collection of Cons[ ] functions.h
  - Speedup on 25-muscle because we aren't proposing the branch lengths badly
  - Eliminate compiler warnings.
  - Print expressions
  - Allow non-modifiable branch lengths if we aren't doing MCMC.
  - Improve pre-burnin
  - read parameter values as json
  - write out run parameters as json.
  - Improve help, add help for more functions.
  - stop finding branch lengths by parameter name
  - stop finding scales by parameter name
  - print parameter values that are data structures
  - meson build infrastructure
  - Read json using nlohmann::json
  - Switch to new ptree structure.

3.0-beta5 (Dec 6, 2017)
  - Fixes
    - Don't crash on --smodel=GTR+x3
  - Short parameter names
  - Add --link option.
  - Complain if linking non-existant attributes.
  - Logging
    - Logging of [(String,a)]
    - Rationalize logging of frequencies and exchangabilities
    - DirichletOn prior for frequencies and exchangabilities.
  - Loading functions from files.
  - Allow expressing the branch length prior in terms of the tree.
  - Automatically convert integers to double when needed, and hide the conversions.
  - Help
    - Make simple|advanced|expert|(developer) options.
    - Help for all commands.
