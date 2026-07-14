# Faster Haskell test-suite investigation plan

This is the reviewed, second-stage plan for reducing the time taken by the
BAli-Phy Haskell tests.  It is deliberately experiment-driven: the scanner
change is tested first because current profiles identify it as the dominant
fixed cost, and each later piece of infrastructure is retained only if it
provides a material additional improvement after the earlier wins are in
place.

The filename has the requested `.hs` suffix, but this file is a Markdown plan,
not a Haskell module.

## Implementation results

The experiments retained four independent changes: generated scanner tables,
two focused test consolidations, and faster executable identification for the
compiled-module cache.  Measurements below used `gcc-16-debug-O`, isolated
writable homes, warm module caches unless stated otherwise, and no more than
seven concurrent tests.

### Retained changes

- The original scanner took 2.52--2.55 seconds, about 29.0 billion
  instructions, roughly 373,000 page faults, and about 1.2 GB RSS to compile
  the two-line `Types/11` test.  Generating full scanner tables with RE/flex
  `-f` reduced that invocation to about 0.13 seconds before the later cache-key
  optimization, 844 million instructions, 574 page faults, and 13.5 MB RSS.
  All 19 lexer tests passed.
- RE/flex `-F` also passed the lexer tests, but executed about 851 million
  instructions and produced a 29.5 MB lexer object instead of the `-f`
  object's 20.7 MB.  Its small wall-time variation was not a material win, so
  the `-F` experiment was abandoned and no scanner-mode option was retained.
- Nine positive `Data.Vector` tests whose warm serial total was 3.27 seconds
  now run as one labelled runtime specification in about 0.40 seconds.  Tests
  of errors, strictness, and native tree paths remain independent.
- Thirteen compatible positive `Data.Array` tests now run as one labelled
  runtime specification.  Its warm time is about 0.40 seconds; 14 error and
  strictness cases remain independent.
- The post-scanner profile exposed hashing the approximately 450 MB executable
  as the new fixed startup cost.  Increasing the fallback read buffer from 4
  KB to 1 MiB reduced the tiny direct test to about 0.067 seconds.  On Linux,
  using the loaded ELF GNU build ID reduced it further to about 4.72 ms and
  27.1 million instructions.  Other platforms retain the full-file hash with
  the larger binary-mode buffer until an exact native build identity is
  available.  Cache migration, retention, compression, warm loading, and
  corrupt-entry recovery tests pass.

### Final suite measurements

The retained stack discovers 313 Haskell tests.  With one prewarmed
executable-specific cache, the complete set passed at each measured
concurrency:

| Jobs | Wall time |
| ---: | --------: |
| 1 | 109.27 s |
| 4 | 30.79 s |
| 7 | 20.70 s |

Seven jobs are therefore retained as the normal upper bound.  The final
parallel run was not limited by the old scanner or executable hashing: its
long tail was real runtime work, including `Numeric/LinearAlgebra` at 14.93
seconds, `Probability/PrimitiveVectorBuiltins` at 6.60 seconds, and
`SMCInterface` at 6.15 seconds.

The final validation run passed all 514 registered BAli-Phy tests, including
the required `bali-phy 5d +A 50` test.

### Experiments not retained

- In-process batching was not implemented.  A direct compiler invocation now
  takes about 5 ms, so even eliminating that entire cost for all 313 tests
  would save under 2% of the 109-second serial suite.  The loader, compiler,
  and runtime also own mutable module maps, source roots, unique state,
  diagnostics, heaps, registers, random state, and native state which would
  need an explicit reset protocol.  This fails the batching gate before that
  infrastructure is justified.
- Local `Main` cache names and cold-cache locking were left alone.
  `--test-module` intentionally recompiles the source under test, shared
  modules load successfully from the executable-specific cache, and the warm
  suite showed no cache-collision failure worth new namespaces or locking.
- A stop-after-frontend-phase interface was not added.  Expected frontend
  failures already stop naturally and complete in about 0.04 seconds through
  Meson after the fixed-cost changes.  A new compiler mode cannot meet the
  required 5% complete-suite improvement on that population.
- The 17 remaining tests without `NoImplicitPrelude` were inspected.  Most
  exercise Unicode syntax or broad Prelude-facing APIs, while three FFI tests
  are part of separate ongoing compiler work.  Making all dependencies
  explicit would lengthen those tests without affecting the now-small fixed
  compiler floor.
- No batch manifest, cache lock, test-support library, stop-phase enum, or
  additional Meson suite labels were left dormant.  The full parallel Haskell
  suite is short enough that suite labels would change feedback selection but
  not address a remaining work bottleneck.

## Current evidence

The starting measurements were made with
`build/gcc-16-debug-O` and show:

- There are currently about 330 Haskell test directories.  Meson registers
  each directory as a separate test, `tests/run-tests.py` starts one Python
  process for the test, and that process starts a fresh `bali-phy` process.
- A two-line `NoImplicitPrelude` module tested with `--test-module` takes about
  2.5 seconds and reaches roughly 1.1 GB maximum resident memory.
- A small runtime test takes about 3.6 seconds, of which approximately 3.4
  seconds is setup and compilation rather than the requested computation.
- `bali-phy --version` completes essentially immediately.  The fixed cost
  begins when the compiler parses a Haskell module; it is not principally
  Python or ordinary process startup.
- A `perf` profile of the two-line test is dominated by `reflex::Pattern`
  construction, including DFA position copying, sorting, comparison, and page
  allocation.
- The generated scanner in `src/computation/parser/lexer.cc` constructs seven
  function-local static `reflex::Pattern` objects from large regular-expression
  strings on their first use.  `REGEX_INITIAL` alone is approximately 779 KB.
- RE/flex supports `--fast` (`-F`), which generates FSM code, and `--full`
  (`-f`), which generates FSM opcode tables.  The current generation command
  uses neither option.
- Approximately 314 of 329 current `Main.hs` files already use
  `NoImplicitPrelude`.  Extending it to the remaining eligible tests may help,
  but cannot remove the measured no-import floor.
- Test entry modules are generally all named `Main`.  The compiled-module
  cache path is based on module identity, while `--test-module` also forces the
  requested module to be recompiled.  Cache changes therefore need measurement
  rather than an assumption that they will help.

These measurements must be repeated when implementation starts because the
working copy and executable may have changed.

## Review findings

Review of the first-stage outline produced the following decisions:

- Attack runtime scanner construction before changing the test harness.  It is
  the only proposed change already supported by a profile, has a small
  interface cost, and may change whether batching or cache infrastructure is
  still worthwhile.
- Treat `-F` and `-f` as competing implementations of one scanner change.  Do
  not retain selectable scanner modes or two generated scanners after choosing
  the better tradeoff.
- Separate reductions in total work from reductions in feedback latency.
  Scanner generation, batching, consolidation, phase stopping, and effective
  caching can reduce CPU work.  Meson suites, test ordering, and CI sharding
  mainly improve latency and do not substitute for the former changes.
- Do not begin with a persistent compiler daemon or a Unix fork server.  A
  daemon introduces a protocol and state-lifetime boundary; a fork server is
  platform-specific and awkward around C++ state and threads.  A bounded batch
  mode is the largest process-reuse mechanism worth considering initially.
- Test batching must preserve isolation of the test-local `Main`, compiler
  uniques, extensions, source roots, error state, and runtime state.  Merely
  looping around the current top-level action without identifying this state
  would make the suite fast but unreliable.
- Do not introduce cache locks, cache namespaces, phase-selection APIs, or a
  general test manifest pre-emptively.  Each is new infrastructure and must
  pass a measured benefit gate below before being kept.
- Consolidate only tests which naturally make one statement.  Negative
  diagnostics, expected crashes, incompatible extensions, and tests of module
  isolation should remain separate unless batch execution can preserve their
  identities.
- Keep at most seven concurrent build or test jobs.  With the current
  approximately 1.1 GB footprint per tiny compiler invocation, increasing
  concurrency before reducing memory use may make wall time worse.
- Preserve full regression coverage.  No test is deleted merely because its
  startup is expensive; consolidation must retain the assertions and expected
  diagnostics represented by the original tests.

## Common experiment protocol

Use this protocol for every candidate so that changes are comparable.

### Revisions and builds

1. Record the starting `jj` revision, `jj status`, compiler version, RE/flex
   version, CPU model, and available memory in the experiment notes.
2. Do not mix this work with unrelated working-copy changes.  When the work can
   begin from a clean parent, create an empty child for each competing
   experiment.  Build `-F` and `-f` from the same parent rather than measuring
   one on top of the other.
3. Build through the default out-of-source directory:

   ```sh
   nice -n10 ninja -j7 -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O
   ```

4. Record the time to regenerate and compile the scanner, the final executable
   size, and the size of the generated scanner object.  A faster test startup
   must not hide an unreasonable recurring build cost.

### Cache states

Use a fresh temporary `XDG_DATA_HOME` for each candidate instead of deleting or
polluting the user's normal module cache.  Give the baseline and each candidate
their own directory.

For each timing set:

1. Run the executable once with `-V` to populate its executable-specific
   module cache and record which modules compile.
2. Discard that first timing.
3. Run the measured command at least five times for microbenchmarks and use the
   median.  Record the range as well, so frequency changes or outliers are
   visible.
4. Use `/usr/bin/time -v` for elapsed time and maximum RSS and `perf stat` for
   task-clock, instructions, cycles, and page faults.  Instruction count is
   the primary comparison when elapsed times disagree with one another.
5. Interleave baseline and candidate measurements if the difference is below
   10%, rather than running all samples of one executable first.

Do not add a permanent benchmark helper initially.  These commands are few
enough to run directly.  Introduce a script only if the same multi-case
protocol is retained as a regression benchmark; if introduced, it must explain
the cache warmup and report the revision and executable hash.

### Representative workloads

Measure these cases individually:

- `tests/haskell/Types/11`: a two-line `NoImplicitPrelude` compile-error test
  using `--test-module`; this isolates the fixed compiler/scanner cost.
- `tests/haskell/Lexer/Unicode/Identifiers`: a Unicode lexer test; this detects
  a fast scanner which mishandles the expensive part of the lexical grammar.
- `tests/haskell/Data/Vector/ReplicateNegative`: a small runtime failure test.
- `tests/haskell/Numeric/LinearAlgebra`: an import-heavier successful runtime
  test.

Define a stable Haskell timing subset before modifying code.  It must contain
all four cases above, at least ten successful runtime cases, at least ten
expected frontend failures, all lexer Unicode cases, and examples from
`Data.Vector`, `Data.Array`, records, type families, and foreign imports.
Store the list in the experiment notes at first.  Make it a Meson suite only if
the suite mechanism is retained later.

Measure both:

- serial execution (`-j1`), which approximates total work; and
- bounded parallel execution (`-j7`), which measures ordinary wall time and
  memory contention.

For a candidate which passes its focused checks, run the complete Haskell test
set once serially for a work comparison and once at `-j7` for wall time.  If a
comparison is within 10%, repeat it in interleaved order.  Before retaining a
compiler or runtime change, also run:

```sh
meson test -C /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O \
  --print-errorlogs 'bali-phy:bali-phy 5d +A 50'
```

The final retained stack must pass the complete test suite, not merely the
timing subset.

### General retention rule

Correctness is a hard gate.  A candidate which changes accepted programs,
diagnostics covered by golden tests, evaluation behavior, or test isolation is
rejected even if it is faster.

Prefer a candidate which removes at least 10% of serial Haskell-suite time or
0.25 seconds from the median tiny-test invocation.  A smaller change may be
kept when it removes code or makes the design simpler, but it does not justify
new persistent infrastructure.  Any infrastructure with its own protocol,
cache policy, or compiler mode must save at least 10% of the workload to which
it applies and at least 5% of the complete serial Haskell suite.

Record rejected experiments and their measurements in this file, then abandon
their changes.  Do not leave dormant options, compatibility paths, or unused
helpers behind.

## Experiment 1: generate scanner automata ahead of time

This is the first concrete implementation experiment and should be completed
before designing other compiler reuse.

### Design analysis

Generating scanner code or tables is preferable to caching a serialized DFA:
RE/flex already owns the generation format, generated code has no cache
validity or locking problem, and every `bali-phy` invocation benefits.  The
costs to measure are generated-source size, C++ compile time, object and
executable size, and any change in scanner behavior.  A hand-written Unicode
scanner is outside scope because it would replace tested lexical semantics
with substantially more custom machinery.

### Candidate A: full table scanner

From an empty child of the baseline:

1. Change `src/computation/parser/gen_parser.sh` to invoke RE/flex with `-f`.
2. Regenerate `src/computation/parser/lexer.cc` through the existing generator;
   do not edit the generated scanner by hand.
3. Confirm that the generated `raw_yylex` no longer constructs the large
   `reflex::Pattern` values from regular-expression strings at runtime.
4. Build and run the common protocol.
5. Run every test below `tests/haskell/Lexer`, including malformed UTF-8,
   Unicode identifiers, comments, pragmas, layout columns, numeric escapes,
   and adjacency cases.
6. Save the measurements, then leave this candidate without stacking Candidate
   B on it.

### Candidate B: fast code scanner

Repeat the same steps from the original baseline with `-F` instead of `-f`.
Use exactly the same tests and measurements.

### Selection

Reject a mode if it fails any lexer or parser regression, still constructs the
large automata at runtime, or makes scanner compilation impractical in the
normal debug-O build.

If both pass:

- Choose the faster mode when its extra generated/object/executable size is no
  more than 25% relative to the other mode.
- If the faster mode is more than 25% larger, keep it only when it improves the
  serial Haskell timing subset by at least another 5% or saves at least another
  0.15 seconds per tiny invocation.  Otherwise choose the smaller mode.
- If neither removes at least 0.25 seconds from `Types/11`, use `perf` again to
  verify whether pattern construction really disappeared.  Reject both and
  investigate generated-matcher selection before doing any batching work.

Retain one normal generation command, regenerate `lexer.cc`, and squash only
the selected candidate into a single commit with a description such as:

```text
Generate the Haskell scanner automaton ahead of time
```

Do not retain an option for choosing between `-f`, `-F`, and the old runtime
pattern construction.

## Gate 2: profile the remaining fixed cost

After selecting the scanner, repeat the common microbenchmarks and take a fresh
`perf record` of `Types/11` and `Numeric/LinearAlgebra`.

Classify the remaining serial timing-subset work into:

1. process and static initialization;
2. construction of compiler primitive modules;
3. loading or compiling shared imported modules;
4. parsing/typechecking the test-local module;
5. simplification/code generation;
6. runtime-machine setup and actual evaluation; and
7. Python/golden-file handling.

Use existing `-V` output and profiler stacks first.  Add temporary scoped
timers only where those two sources cannot separate adjacent compiler phases,
and remove the timers after recording the result.

The resulting table determines which experiments below are run:

- Run batching only if process/static initialization plus reconstructing shared
  compiler state is at least 10% of the serial Haskell subset after the scanner
  change.
- Run the cache experiment only if shared imported modules are visibly
  recompiled or repeatedly read from disk and consume at least 5% of the
  subset.
- Run the stop-after-phase experiment only if successful frontend tests spend
  at least 10% of their time after their intended checking phase.
- Consolidation can proceed independently where multiple tests naturally form
  one specification.

This gate prevents the later infrastructure from being implemented merely
because it was useful under the old scanner profile.

## Experiment 2: consolidate naturally related tests

This experiment reduces the number of complete compiler invocations without
changing compiler architecture.

### Pilot

Choose one cluster of at least eight successful, closely related tests from
`tests/haskell/Data/Vector` or `tests/haskell/Numeric/LinearAlgebra`.  Before
editing, write a checklist containing every old test's command, expected
output, expected exit status, extensions, and assertion being made.

Create one replacement `Main.hs` only when all selected cases:

- have compatible extensions and package paths;
- can run successfully in one process;
- do not test process, module, or compiler-state isolation; and
- can retain a labelled assertion or labelled output for each old behavior.

Use `NoImplicitPrelude` and minimal imports.  Do not add a general assertion
library for this pilot.  Preserve laziness and exception tests: a case which
must terminate the process remains an independent test.

Run every old case before the edit, the replacement after the edit, and the
surrounding subsystem tests.  Compare the serial sum of the old test times with
the replacement time.  Keep the consolidation if it retains every checklist
item and saves at least 25% for the cluster.  Otherwise restore the separate
tests.

### Expansion

If the pilot wins, apply the same checklist procedure to other positive
microtest clusters.  Keep negative diagnostic cases separate unless a later
batch mode preserves independent diagnostics.  Each subsystem consolidation
is a separate commit, for example:

```text
Consolidate Data.Vector runtime tests
```

Do not create one giant Haskell test: subsystem-level modules retain useful
failure localization and allow Meson to run unrelated subsystems in parallel.

## Experiment 3: bounded in-process batching

Run this experiment only if Gate 2 shows enough remaining reusable setup cost.

### Design analysis and state inventory

Before writing a batch interface, trace one `--test-module` invocation from
`src/bali-phy/bali-phy.cc` through `module_loader`, `Program`, typechecking, and
module execution.  Produce a table for these objects:

- ownership and lifetime;
- whether they are immutable after compiler startup;
- whether a second test can reuse them;
- the operation which resets or replaces test-local state; and
- an existing test which would detect leakage.

The table must cover the test-local `Main`, source and package search paths,
loaded module map, extension settings, unique-variable state, diagnostics,
simplifier state, runtime registers/heaps, random seed, and native builtin
state.  This is the required design analysis for the batch helper.  If these
objects cannot be reset through existing ownership boundaries, stop: do not
build a parallel test-only compiler architecture.

### Feasibility prototype

In a temporary child revision, add the smallest internal-only driver capable
of running a list of successful `--test-module` jobs in one process.  The
prototype may use a fixed list supplied on the command line and may report only
one aggregate status; it is for measuring reuse and is not the final protocol.

Measure three variants over 20 successful compile-only tests:

1. current separate processes;
2. one process but a fresh `Program`/loader for every test; and
3. one process reusing only the immutable/shared objects identified by the
   state inventory, while replacing all test-local state.

After each test in variants 2 and 3, compile a sentinel module which would fail
if the preceding test's `Main`, extension set, source path, or unique bindings
remain visible.  Run the 20 jobs in forward and reverse order and require the
same results.

Remove the prototype if the best safe variant does not improve those 20 tests
by at least 15% after the scanner change, or if correct reset requires a second
parallel compilation pipeline.

### Retained batch interface

If the prototype passes the gate, replace it with a bounded manifest mode used
only by the test harness:

- A manifest entry contains the test directory, argument vector, expected
  mode (`compile` or `run`), and paths for captured stdout, stderr, and exit
  status.  Use a versioned, length-delimited format or an existing structured
  parser; do not parse shell command strings in C++.
- The compiler executes entries sequentially and writes an explicit result for
  every entry, even after an ordinary compile error.  A compiler crash still
  fails the whole chunk.
- Start with chunks of 16 tests.  Generate chunks deterministically by sorted
  test path and do not mix tests requiring incompatible external setup.
- `tests/run-tests.py` remains responsible for comparing golden files and xfail
  status.  Do not duplicate comparison rules in C++.
- Meson registers each chunk as a test and includes all member paths in the
  displayed name or failure output.
- On failure, print the exact standalone command needed to rerun the individual
  member outside batch mode.
- Keep standalone execution working; it is the debugging path, not a
  compatibility wrapper around batching.

First batch successful compile-only tests.  Add successful runtime tests only
after the state inventory proves that the runtime machine and random seed are
fresh per entry.  Continue to run expected crashes individually.  Negative
compile tests may join batches only when diagnostics are captured separately
and processing demonstrably resumes after an ordinary error.

Compare chunk sizes 8, 16, and 32 on the stable timing subset.  Select the
smallest size within 5% of the fastest result, limiting the amount of work lost
to a crash and retaining parallelism.

Keep batching only if it saves at least 10% of the complete serial Haskell
suite after scanner generation and consolidation.  Land the compiler batch
operation and its immediate harness use in one commit so no unused protocol is
introduced:

```text
Batch Haskell tests within compiler processes
```

## Experiment 4: make test module caching effective

Run this only if Gate 2 identifies cache lookup or shared-module compilation as
material after the scanner change.

### Establish the failure mode

With an isolated cache and `-V`:

1. Run four different tests named `Main` sequentially, then repeat them in the
   same order.
2. Record cache paths read and written, modules recompiled, artifact sizes, and
   whether the second pass obtains a hit for shared modules and for each local
   `Main`.
3. Run four import-heavy tests simultaneously on a cold cache and determine
   whether they compile the same shared modules independently.
4. Repeat after prewarming one import-heavy test serially.

This distinguishes three different problems: intentional `--test-module`
recompilation, local-`Main` filename collisions, and a cold-cache thundering
herd.  Do not solve one as though it were evidence for all three.

### Conditional fixes

- If only `--test-module` forces the local entry module, retain that behavior:
  diagnostic tests need to compile their current source.  Do not weaken it for
  a benchmark.
- If local modules with the same textual name overwrite otherwise reusable
  artifacts, change only local/source modules to include a stable hash of
  canonical source identity in the artifact path.  Package modules retain
  their current shared identity.  Verify that moving or modifying a source
  cannot load a stale artifact and record the disk-space increase for all
  Haskell tests.
- If parallel cold starts duplicate shared compilation and serial prewarming
  removes at least 10% of cold-suite time, first implement a Meson fixture which
  warms the shared package modules once.  Prefer this to cache locking because
  it has no cross-process failure protocol.
- Add per-artifact locking only if prewarming cannot cover the duplicate work.
  A lock design must specify stale-lock recovery, atomic publication, process
  failure, and Windows behavior before implementation.  Waiting processes must
  validate and load the published artifact rather than compiling it again.

Keep a cache change only if it saves at least 10% of cold Haskell-suite time or
at least 5% of the normal warm suite, and does not create unbounded per-test
cache growth.  Give source identity and cold-cache coordination separate
commits if both are retained; they make separate statements.

## Experiment 5: stop after the phase under test

Run this only if Gate 2 finds material work after the intended phase for
successful frontend tests.

### Design analysis

Inventory existing compiler phase boundaries and identify the single normal
pipeline operation after lexing, parsing, renaming, typechecking, and Core
generation.  A stop mode is acceptable only if it returns through the normal
pipeline at one of those boundaries.  Do not add a test-only parser or
typechecker entry point.

Classify existing frontend tests by the last phase required to establish their
assertion.  An expected error already stops naturally and must not be relabelled
unless measurement shows extra recovery work.  Optimizer, code-generation,
foreign ABI, interpreter, and runtime tests remain full-pipeline tests.

### Prototype and retention

Add one internal `StopAfter` enum threaded through the normal compiler driver,
initially supporting only the phase with the largest measured population.  Add
a command-line spelling only for testing.  Convert ten successful tests whose
purpose unambiguously ends at that phase and compare their diagnostics and
timings with full compilation.

Keep the mode only if those tests improve by at least 10% and the complete
serial suite improves by at least 5%.  If retained, add only phase values with
current users, document that stopping does not validate later compiler stages,
and convert the remaining unambiguous tests.  Land the pipeline mode and its
first users together:

```text
Stop frontend tests after their required compiler phase
```

If the gate fails, remove the enum and command-line option rather than keeping
an unused general facility.

## Experiment 6: remaining low-cost test cleanup

After the structural experiments, inspect the approximately 15 remaining
Haskell tests without `NoImplicitPrelude`.

- Add it where the test does not deliberately exercise the implicit Prelude
  and can name its small set of imports directly.
- Do not change tests whose purpose includes Prelude lookup, defaulting, or
  implicit imports.
- Do not introduce a new test-support module unless at least ten tests would
  use it and compiling that shared module is measurably cheaper than their
  existing imports.
- Compare each affected cluster before and after.  Retain conversions which
  simplify dependencies without changing the assertion, even if their timing
  effect is small; do not claim them as a major speedup.

This cleanup is one separate commit if it has more than incidental changes.

## Experiment 7: concurrency, suites, and feedback latency

Perform this after the selected work-reduction changes, because their memory
footprint determines useful concurrency.

### Concurrency

Run the stable subset at `-j1`, `-j2`, `-j4`, and `-j7`.  Record wall time,
aggregate task-clock, maximum combined RSS, page faults, and any swapping.  Run
the complete Haskell suite at the best two settings.  Use no more than seven
tasks across all simultaneous jobs.

Choose the lower concurrency when it is within 5% of the fastest wall time;
this leaves memory and CPU capacity for other work.  Encode a CI concurrency
change only if it is faster on the CI machine as well as the development
machine.

### Meson suites

If full-suite time remains large enough to impede ordinary development, add
Meson suite labels derived from the existing test paths:

- `haskell-smoke` for a small representative set;
- `haskell-frontend` for lexer, parser, renamer, and typechecker tests;
- `haskell-runtime` for interpreter and builtin behavior; and
- `haskell-full` for every Haskell test.

The full membership must be generated from the same discovered test list as
the current Meson registrations; do not maintain a second hand-written list.
The smoke suite contains the stable timing subset and must finish quickly
enough to be useful locally.  Full CI continues to run `haskell-full`.

Suites improve feedback latency rather than total work, so land them in a
separate commit and describe them accordingly.  Add CI sharding only if one
full Haskell job remains a wall-time bottleneck after batching and concurrency
tuning.  Shards must be deterministic, balanced using recorded test durations,
and together run exactly the full discovered set.

## Final integration and commit structure

Retain only experiments which pass their gates.  The likely commit sequence is:

1. generate one selected scanner automaton representation;
2. consolidate each worthwhile subsystem test cluster;
3. batch tests, only if its post-scanner benefit justifies the protocol;
4. improve local module cache identity and/or cold-cache coordination, only for
   independently demonstrated problems;
5. stop frontend tests at a normal phase boundary, only if it provides a
   suite-level win;
6. clean up remaining eligible implicit-Prelude tests; and
7. add suites or tune CI parallelism as a feedback-latency change.

Each retained statement gets its own `jj` commit.  Experimental children which
lose a comparison are abandoned.  If a retained experiment exposes a bug in
the recent scanner or cache commit which introduced it, fix it in a fresh child
of that commit and squash it back once complete rather than appending an
unrelated fix commit.

For the final retained stack:

- rebuild with `nice -n10 ninja -j7` in `gcc-16-debug-O`;
- run every focused regression named above;
- run all Haskell tests serially and at the selected bounded concurrency;
- run the complete BAli-Phy test suite;
- run `bali-phy 5d +A 50`;
- repeat the four representative measurements with warm executable-specific
  caches; and
- compare total serial Haskell time, `-j7` wall time, instruction count, page
  faults, peak RSS, executable size, and scanner rebuild time against the
  recorded baseline.

Update the evidence and decisions in this file with the measured results.  The
work is complete when all retained changes are exercised by the normal test
path, rejected infrastructure has been removed, full coverage passes, and the
final report can attribute the speedup to specific retained changes rather
than to cache warmth or CPU-frequency noise.
