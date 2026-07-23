# Coalescent benchmark history

This tracks performance changes for:

```
bali-phy -m Model.hs RSV2-25taxa.fasta --iter=N --name ignore --seed=S
```

## Method

- A **controlled boundary** compares a parent and child, or common-parent
  siblings, built identically.  Matching logs and trees control the stochastic
  trajectory and support attributing the total performance change to that
  boundary; identifying the internal mechanism still requires rollback or
  diagnostic evidence.
- A **distributional checkpoint** is required when behavior changes.  A fixed
  seed does not fix the workload if draw consumption or accept/reject decisions
  diverge.  Compare a fixed panel of seeds, retain each result, and report the
  mean and dispersion rather than attributing a single-seed difference to the
  source change.
- Use GCC 16 release builds with the same options, ISA, libraries, model, and
  input.  A shipped binary built for another ISA answers a separate question.
  Give each revision a dedicated `HOME` and output directory, warm it once, and
  verify `.mod` cache hits; generated `Main.hs` may change with output paths.
- Record the exact `jj` source revision.  When `jj edit` represents that commit
  as its Git parent plus a working-tree diff, the executable's embedded Git
  version may name the parent even though the measured sources are the `jj`
  revision.
- Measure `perf stat` instructions at `N=1` and `N=50` for each seed.
  `(I50-I1)/49` estimates recurring work within a revision; cross-revision
  differences are controlled only when traces match.  Use cycles, Work, and
  at least seven interleaved timings as supporting evidence; report medians
  and retain the individual runs.
- Keep verbose graph generation and diagnostic instrumentation out of measured
  builds.  Instrument a separate variant to test a mechanism, then benchmark
  the corresponding uninstrumented baseline, rollback, and fix.
- Retain one raw record per process invocation with revision, host and build
  identity, command and input, seed, iteration count, cache status, counters,
  timing, run order, log and tree hashes, and instrumentation status.  Raw
  records belong in `benchmarks/coalescent-runs.tsv`; a plain TSV is sufficient
  and requires no benchmark executable.
  Summaries must state the seed set, trace status, and evidence class.

Prefer coherent design endpoints over migration intermediates.  Record
individual regressions and recoveries separately: a later net improvement does
not show that an earlier regression was fixed.

Tables use **controlled** for a same-trace boundary, **distributional** for a
multi-seed comparison after traces diverge, and **checkpoint** for an absolute
or single-seed measurement that does not support causal attribution.
**Hypothesis** marks a proposed mechanism that has not yet been tested.

The initial history search reused one cache directory and did not retain every
raw run.  In the table, `verified` means that imported modules were observed to
hit a dedicated warmed cache; five early rows remain `unverified`.  Trace
status is `unknown` unless explicitly recorded.  Unverified and unknown results
still locate transitions, but close comparisons should be remeasured.

## Single-seed history

### Measurements

These measurements use seed 0.  Empty `I1` and `Extra/iter` cells were not
measured.  Numeric differences are interpreted separately below so that the
measurement table can remain compact.

| Revision | Change boundary | I1 (B) | I50 (B) | Extra/iter (B) | Work 50 (s) | Cache |
|---|---|---:|---:|---:|---:|---|
| `f30c3177` | 4.2 source revision, common build | 15.756 | 99.917 | 1.718 | 8.802 | verified |
| `bed6443e` | Before `runtime/ast.H` | 16.238 | 103.983 | 1.791 | 9.615 | verified |
| `0053b4f0` | `std::variant` Runtime Exp | 15.570 | 108.871 | 1.904 | 11.016 | verified |
| `8a647583` | Variant2 Runtime Exp | 15.616 | 110.670 | 1.940 | 10.863 | verified |
| `38bd4274` | Before newtype IO | | 111.230 | | 9.658 | unverified |
| `c8327966` | Use newtype for IO | | 103.030 | | 8.858 | unverified |
| `06e65341` | Runtime app layout series | | 103.559 | | 9.073 | unverified |
| `0f86a50a` | June 22 checkpoint | | 103.285 | | 8.987 | unverified |
| `0dd9d033` | Before RE/flex switch | 16.602 | 102.973 | 1.763 | 8.896 | verified |
| `b9676b26` | Before Unicode identifiers | 17.085 | 103.188 | 1.757 | 8.823 | verified |
| `682b647a` | Unicode identifiers | 37.444 | 123.570 | 1.758 | 9.414 | verified |
| `692736b8` | Before Map/Set rewrite | 39.046 | 125.174 | 1.758 | 9.351 | verified |
| `19aac6be` | June 28 checkpoint | | 125.202 | | 9.363 | unverified |
| `90a02246` | Before dependent USE/FORCE edges | 38.979 | 123.824 | 1.732 | 9.455 | verified |
| `3329ae5f` | Dependent-edge machinery | 39.056 | 126.919 | 1.793 | 9.982 | verified |
| `d980f9b0` | Direct list-to-`EVector` conversion | 38.632 | 121.858 | 1.698 | 8.995 | verified |
| `1f3aab48` | Direct `IntMap.restrictKeysToVector` | 39.152 | 135.225 | 1.961 | 10.345 | verified |
| `4cdcf512` | Dependent USEs in `IntMap` export | 39.162 | 135.222 | 1.960 | 10.368 | verified |
| `672da381` | Dependent USEs in `IntMap.toVector` | 39.913 | 177.742 | 2.813 | 15.757 | verified |
| `1f2e3775` | Direct string packing | 39.893 | 177.729 | 2.813 | 16.474 | verified |
| `028bdd59` | Before native Int result changes | 41.630 | 170.585 | 2.632 | 15.241 | verified |
| `f5a54cba` | Before derived native Int lengths | 41.629 | 170.604 | 2.632 | 15.448 | verified |
| `ca624c91` | Derive native Int lengths | 41.210 | 190.803 | 3.053 | 17.540 | verified |
| `16030d4e` | Native-vector span endpoint | 41.193 | 190.654 | 3.050 | 17.295 | verified |
| `967d1abb` | Before type-family/FFI series | 40.885 | 190.323 | 3.050 | 17.365 | verified |
| `312c93ff` | Before precompiled scanner | 41.321 | 190.838 | 3.051 | 17.555 | verified |
| `42220746` | Precompile scanner | 19.447 | 168.825 | 3.049 | 16.579 | verified |
| `97554a37` | Completed NonRec series | 18.721 | 167.472 | 3.036 | 16.178 | verified |
| `a2dcb8a5` | Completed arity/eta series | 18.789 | 150.380 | 2.686 | 14.414 | verified |
| `93b016a0` | Before NativeVector translations | 18.781 | 150.394 | 2.686 | 14.328 | verified |
| `62beaa4d` | Before eigen/vector results | 18.791 | 150.432 | 2.687 | 14.499 | verified |
| `656546a3` | Eigen-exponential vector arguments | 18.804 | 150.413 | 2.686 | 14.361 | verified |
| `f79dffc6` | Lazy `Vector` length | 19.115 | 142.819 | 2.525 | 13.370 | verified |
| `b0361903` | Lazy `Matrix` dimensions | 19.045 | 135.413 | 2.375 | 12.947 | verified |
| `ed5611f3` | Completed vector translations | 19.083 | 135.441 | 2.375 | 12.763 | verified |
| `670ccb6f` | Before direct coalescent inputs | 19.131 | 135.498 | 2.375 | 12.807 | verified |
| `b0f1cb92` | Direct coalescent inputs | 17.704 | 115.584 | 1.998 | 10.710 | verified |
| `02ea5034` | Before edge-contingency API | 19.117 | 134.317 | 2.351 | 12.500 | verified |
| `85415916` | Windows-test plan checkpoint | 19.132 | 134.347 | 2.351 | 12.713 | verified |
| `44e6c6d9` | Edge-contingency API only | 19.150 | 135.568 | 2.376 | 12.594 | verified |
| `ee18ab15` | IntMap edge contingency | 18.251 | 101.739 | 1.704 | 8.550 | verified |
| `258d2fd4` | Other important users converted | 18.180 | 98.468 | 1.639 | 8.292 | verified |
| `eae8edab` | Legacy dependent API removed | 18.153 | 98.207 | 1.634 | 8.201 | verified |
| `03548c24` | Before direct coalescent node-time inputs | 18.164 | 98.227 | 1.634 | 9.035 | verified |
| `2ce355ee` | Direct coalescent node-time inputs | 17.437 | 96.261 | 1.609 | 8.920 | verified |

### Evidence and interpretation

Trace status and evidence compare each row with the preceding relevant
checkpoint unless another revision is named.  `unknown` means that no trace
comparison was retained, not that the traces differed.  Some later compiler
and lazy-dimension boundaries were observed to change the trace, but the
affected rows were not recorded and therefore remain `unknown` here.

| Revision | Trace | Evidence | Interpretation |
|---|---|---|---|
| `f30c3177` | n/a | checkpoint | Source-built 4.2 baseline, not the downloaded `x86-64-v3` binary.  An earlier three-run Work mean was 9.069s. |
| `bed6443e` | n/a | checkpoint | Start boundary for the Runtime AST migration. |
| `0053b4f0` | same | controlled | The completed `std::variant` endpoint has 6.3% more recurring work than `bed6443e`, despite reducing fixed startup work. |
| `8a647583` | same | controlled | The direct Variant2 change adds another 1.9% recurring work; the completed endpoint is 8.3% above `bed6443e`. |
| `38bd4274` | n/a | checkpoint | Start boundary for the IO representation change. |
| `c8327966` | unknown | checkpoint | `I50` is 7.4% lower than `38bd4274`; attribution to newtype IO is not controlled. |
| `06e65341` | unknown | checkpoint | `I50` is 0.5% above the newtype-IO checkpoint. |
| `0f86a50a` | unknown | checkpoint | Whole-process cost remains near the post-newtype level. |
| `0dd9d033` | n/a | checkpoint | Independently warmed checkpoint before the scanner changes. |
| `b9676b26` | n/a | checkpoint | Parent of the Unicode-identifier boundary. |
| `682b647a` | unknown | checkpoint | `I1` rises 20.36B while `Extra/iter` is unchanged, consistent with the generated runtime regex adding fixed startup work. |
| `692736b8` | unknown | checkpoint | The seed-0 recurring estimate is unchanged from `682b647a`. |
| `19aac6be` | unknown | checkpoint | `I50` is similar to `692736b8`; `I1` was not measured. |
| `90a02246` | unknown | checkpoint | The seed-0 recurring estimate is 1.5% below `692736b8`; attribution across the cleanup series is not controlled. |
| `3329ae5f` | unknown | checkpoint | The seed-0 recurring estimate is 3.6% above `90a02246`. |
| `d980f9b0` | unknown | checkpoint | The seed-0 recurring estimate is 5.3% below `3329ae5f`; this does not establish recovery of the same work. |
| `1f3aab48` | changed | checkpoint | Seed-0 recurring cost rises 15.4%, but the changed trace prevents causal attribution. |
| `4cdcf512` | unknown | checkpoint | Seed-0 measurements are essentially unchanged from `1f3aab48`. |
| `672da381` | same | controlled | Byte-identical logs and trees support attributing the 43.5% recurring regression to this boundary; the mechanism remains unproven. |
| `1f2e3775` | unknown | checkpoint | Seed-0 recurring instructions are unchanged from `672da381`. |
| `028bdd59` | unknown | checkpoint | Seed-0 recurring cost is 6.4% below `1f2e3775`; the responsible changes are not isolated. |
| `f5a54cba` | unknown | checkpoint | Seed-0 measurements match `028bdd59`. |
| `ca624c91` | changed | checkpoint | The seed-0 discontinuity is not a controlled delta; the five-seed comparison appears below. |
| `16030d4e` | unknown | checkpoint | Seed-0 measurements match the post-`ca624c91` checkpoint. |
| `967d1abb` | unknown | checkpoint | Seed-0 measurements remain near `16030d4e`. |
| `312c93ff` | unknown | checkpoint | Seed-0 recurring cost remains near the preceding checkpoints. |
| `42220746` | unknown | checkpoint | `I1` falls 21.87B while recurring cost is unchanged, consistent with precompiled scanner automata removing startup work. |
| `97554a37` | unknown | checkpoint | Seed-0 recurring cost is essentially unchanged across the completed NonRec series. |
| `a2dcb8a5` | unknown | checkpoint | Seed-0 recurring cost is 11.5% below `97554a37`; this is not a controlled delta. |
| `93b016a0` | unknown | checkpoint | Seed-0 measurements are unchanged from the arity/eta endpoint. |
| `62beaa4d` | unknown | checkpoint | Early argument translations are neutral in the seed-0 measurements. |
| `656546a3` | unknown | checkpoint | Vector argument translation is neutral in the seed-0 measurements. |
| `f79dffc6` | unknown | checkpoint | Seed-0 recurring cost falls 6.0% at the lazy-`Vector`-length boundary; source inspection explains the avoided force, but the performance delta is not controlled. |
| `b0361903` | unknown | checkpoint | Seed-0 recurring cost falls 5.9% at the lazy-`Matrix`-dimensions boundary; the performance delta is not controlled. |
| `ed5611f3` | unknown | checkpoint | Later vector translations are neutral in the seed-0 measurements. |
| `670ccb6f` | unknown | checkpoint | Matrix translations are neutral in the seed-0 measurements. |
| `b0f1cb92` | unknown | checkpoint | `I50` is the mean of two clean-cache seed-0 runs; recurring cost is 15.9% below `670ccb6f`, but the trace comparison was not retained. |
| `02ea5034` | n/a | checkpoint | Baseline for the controlled edge-contingency migration series. |
| `85415916` | same | controlled | This requested checkpoint changes only the Windows-test plan; recurring instructions differ from `02ea5034` by 0.01%. |
| `44e6c6d9` | same | controlled | Adding the API before migrating users raises recurring instructions by 1.0%; the internal cause is not isolated. |
| `ee18ab15` | same | controlled | Propagating map and key-set contingency through IntMap operations lowers recurring instructions by 28.3% from `44e6c6d9`. |
| `258d2fd4` | same | controlled | Converting native and boxed traversals plus categorical sampling lowers recurring instructions by another 3.8%; this endpoint does not partition the gain by user. |
| `eae8edab` | same | controlled | Removing the legacy API lowers recurring instructions by another 0.3%. |
| `03548c24` | same | controlled | This common-parent benchmark checkpoint matches the completed contingency migration. |
| `2ce355ee` | changed | distributional | Four of five panel traces changed; the five-seed comparison appears below. |

## Multi-seed checkpoints

The retained distributional summaries are means over seeds 0 through 4.  Older
comparisons lack individual results and dispersion; the coalescent node-time
comparison follows the current protocol and has raw records.

| Comparison | Revision | State | I1 (B) | I50 (B) | Extra/iter (B) | Work 50 (s) |
|---|---|---|---:|---:|---:|---:|
| Native Int lengths | `f5a54cba` | before | | 163.811 | | 14.529 |
| Native Int lengths | `ca624c91` | after | | 163.604 | | 14.377 |
| Long-range | `f30c3177` | 4.2 source baseline | 15.639 | 93.602 | 1.591 | 8.216 |
| Long-range | `90a02246` | before dependent edges | 38.396 | 119.348 | 1.652 | 8.779 |
| Long-range | `b0f1cb92` | direct coalescent inputs | 18.076 | 114.380 | 1.965 | 10.387 |
| Coalescent node times | `03548c24` | before | 18.003 | 100.312 | 1.680 | 9.131 |
| Coalescent node times | `2ce355ee` | after | 17.666 | 95.704 | 1.593 | 8.842 |

At the native-length boundary, the mean `I50` and Work changes are small, but
the missing dispersion prevents concluding that the change is systematically
neutral.  The parent graph has 216 each of `Pair:c_fst` and `Pair:c_snd`; the
child instead has 221 `NativeVector:intVectorSize` operations, 216 fed by
`IntSet:keys` and 5 by `IntMap:keys`.  The child does not duplicate the native
key producers.  The graph change perturbs the MCMC trace, and seed 0 happens to
perform substantially more work in the child.

In the long-range panel means, recurring cost at `b0f1cb92` is 23.5% above the
4.2 source baseline, and about 84% of that absolute gap accumulated after the
pre-dependent-edge checkpoint.  Mean Work is 26.4% above 4.2.  These percentages
describe distributional checkpoints, not controlled same-work deltas.  The
current edge-contingency baseline is measured in a separate panel below; its
traces cannot be compared directly with this older panel.

Reading coalescent node times directly from the `IntMap`, with sibling element
USEs contingent only on the map, lowers mean recurring instructions by 5.2%
and mean Work by 3.2%.  Recurring-work standard deviations are 0.036B before
and 0.035B after; the paired change is -0.087B with standard deviation 0.049B.
Work standard deviations are 0.343s before and 0.428s after; the paired change
is -0.289s with standard deviation 0.481s.  Four of five traces changed, so
this is favorable distributional evidence rather than a controlled delta.

## Edge-contingency result

The earlier `672da381` regression came from treating IntMap entry USEs as one
call-order-dependent sequence.  In `ee18ab15`, sibling entries instead inherit
only the map and key-set contingencies.  With byte-identical traces, this lowers
recurring instructions by 28.3% from the API-only checkpoint and by 27.5% from
the pre-API baseline.  This is strong evidence for overbroad dependency
ownership as the main mechanism, although the combined commit does not
partition the recovery among the individual IntMap operations.

Converting the other important traversals recovers another 3.8%, and deleting
the compatibility API recovers 0.3%.  The API-only boundary itself adds 1.0%
recurring instructions and remains unexplained.  Raw measurements and hashes
are in `benchmarks/coalescent-runs.tsv`.

## Runtime AST result

Fresh warmed measurements reproduce the Runtime AST regression with
byte-identical logs and trees.  From `bed6443e` to the completed Variant2
endpoint, fixed startup falls by 0.622B instructions, but recurring work rises
8.3%.  The direct `std::variant` to Variant2 boundary accounts for 1.9%; using
`std::variant` still leaves a 6.3% recurring regression.

Retired-instruction profiles show nearly unchanged absolute work in the main
incremental-evaluation and invalidation routines.  The added work is in runtime
value plumbing: operation argument lookup, object casts, expression
copy/destruction, and register-reference traversal.  Repeated pair-field
extraction in the historical coalescent sort comparator is one concrete
hotspot; `2ce355ee` also removes it by storing detached times before sorting.
The casts are below the variant: runtime heap objects share the `ObjectValue`
alternative, so a same-API outer representation does not remove them.  Hardware
counters show more branches and cache accesses but similar miss rates,
so the regression is primarily extra work.  `Runtime::Exp` and the old
`expression_ref` are both 16 bytes; their containing closures are both 80 bytes.

A same-API `std::variant` experiment on current code reduces recurring
instructions by 2.4%, but three interleaved N=50 runs average 66.118B cycles
and 12.229s task time, versus 64.204B and 11.869s for Variant2.  The replacement
is about 3% slower despite retiring fewer instructions.  A manual tagged union
would duplicate copy, move, destruction, visitation, and serialization logic
for 18 alternatives without evidence that dispatch is the dominant remaining
cost, so it is not introduced.

### Runtime value plumbing experiments

These seed-0 medians use seven interleaved, warmed runs at each iteration count,
with one dedicated cache per endpoint, CPU 11 pinned, and ASLR disabled.  Logs
and trees are byte-identical across all endpoints.  Timing medians are useful
for scale but are not paired estimates.

| Endpoint | I1 (B) | I50 (B) | Extra/iter (B) | Cycles 50 (B) | Task 50 (s) | Result |
|---|---:|---:|---:|---:|---:|---|
| Baseline `191fe271` | 17.500 | 96.282 | 1.608 | 55.250 | 10.345 | reference |
| Retain alignment vectors `6efecf6a` | 17.501 | 96.286 | 1.608 | 55.231 | 10.347 | retained |
| Static required-object access `1e0f6125` | 17.499 | 96.185 | 1.606 | 55.427 | 10.379 | retained |
| Switch on reference tag, uncommitted | 17.500 | 96.307 | 1.608 | 55.644 | 10.421 | rejected |
| Direct required-reference decoder `21ca9c70` | 17.503 | 96.321 | 1.609 | 55.079 | 10.313 | rejected |
| Direct e-op argument staging `585554bb` | 17.485 | 95.468 | 1.592 | 54.831 | 10.270 | rejected |

Retaining an alignment child vector removes an unnecessary O(n) copy in two
alignment builtins, although this benchmark scarcely exercises that path and
shows no measurable effect.  Static access for required `ObjectValue` types
removes redundant migration branches and release-build RTTI; recurring
instructions fall 0.13%, but an elapsed-time improvement is not established.

The tag-switch decoder is worse because GCC already combines the original tag
tests and reference extraction into a shorter branch sequence.  Its exact
source revision was not retained, so this rejected result is diagnostic rather
than a reproducible boundary.  The direct required-reference decoder duplicates
that logic and, in a separate seven-run paired attribution batch, adds median
N=50 task time of 1.74% and cycles of 1.68% when applied after direct e-op
staging.

Direct e-op staging removes a temporary vector and lowers recurring
instructions by about 0.9%, but the elapsed result does not follow the
instruction count.  Across 21 controlled paired samples, its median task and
cycle changes are +0.19% and +0.15%, with 8 wins, 2 ties, and 11 losses in task
time when changes within 0.1% are treated as ties.  It is therefore rejected.
These four investigations do not recover a material part of the completed
Runtime AST regression, so changing the outer `Runtime::Exp` representation
remains unsupported by this evidence.  All 140 primary and attribution
invocations are recorded in `benchmarks/coalescent-runs.tsv`.

### Context-dependent logger overhead

These seed-0 medians compare the context-logger commit `795a9a16` with its
parent `f7465310` using seven interleaved, warmed runs.  Both installed GCC 16
release builds used dedicated caches, CPU 11, and disabled ASLR.  Enabled log
files are byte-identical across the boundary.

| Logging | Endpoint | I1 (B) | I50 (B) | Extra/iter (B) | Cycles 50 (B) | Task 50 (s) |
|---|---|---:|---:|---:|---:|---:|
| enabled | parent | 17.678 | 98.501 | 1.64943 | 56.331 | 10.524 |
| enabled | context logger | 17.677 | 98.503 | 1.64951 | 55.678 | 10.408 |
| disabled | parent | 17.529 | 97.944 | 1.64113 | 55.088 | 10.298 |
| disabled | context logger | 17.529 | 97.937 | 1.64099 | 54.463 | 10.184 |

Recurring instructions change by +0.005% with logging and -0.008% without it,
so the new two-phase interface adds no measurable recurring work.  The roughly
1% cycle and task-time differences are not supported by instruction counts and
remain within timing variation.  All 56 invocations are recorded in
`benchmarks/coalescent-runs.tsv`.

## Slowdowns to investigate

- Edge-contingency API-only boundary: a same-trace 1.0% recurring increase;
  separate dispatch, object-layout, and code-layout effects.
- The earlier standalone `IntMap.restrictKeysToVector` boundary changed its
  trace.  The combined IntMap recovery does not identify its individual share.
- Apparent dependent-edge machinery boundary: 3.6% seed-0 recurring increase
  with unknown trace status; separate bookkeeping, invalidation, and evaluation
  costs.
- Runtime AST conversion: 8.3% more recurring work; Variant2 explains 1.9%.
  Four value-plumbing experiments found only a 0.13% retained instruction
  reduction and no demonstrated elapsed-time recovery.
- Fixed startup at `b0f1cb92`: the five-seed mean `I1` is 15.6% above 4.2
  after removing the Unicode scanner cost; recover the per-seed dispersion.
- The long-range panel mean has a remaining 3.8% recurring increase from 4.2 to
  the pre-dependent-edge checkpoint, including the small runtime-layout
  regressions.

## Future work

- If per-operation attribution is needed, benchmark sibling changes that
  migrate each IntMap operation separately from the API-only checkpoint.
- For genuinely sequential dependencies, count fixed-input fallbacks,
  all-unchanged dependent replays, and first mismatch positions before adding
  replay machinery.
- Backfill raw `I1` and `I50` rows, dispersion, and trace hashes for the
  high-priority changed or unknown boundaries: `restrictKeysToVector`,
  arity/eta expansion, lazy vector and matrix dimensions, and direct coalescent
  inputs.
- Profile expression copy/destruction and operation dispatch before
  reconsidering a custom `Runtime::Exp` representation; object access,
  argument staging, and register-reference traversal have now been isolated.
- Measure `I1` around the IO-newtype series.
- Compare baseline and `x86-64-v3` builds at both 4.2 and current so source and
  instruction-set effects remain separate.
- Compare profiles after normalizing by useful work such as values exported,
  likelihood evaluations, and branches peeled.

## Migration-only checkpoints

These show the shape of the Runtime AST migration.  Intermediate compatibility
and correctness scaffolding can distort their cost, so they are not used to
assess the completed design.

| Revision | Migration state | I50 (B) | Work 50 (s) |
|---|---|---:|---:|
| `3e97da17` | Runtime values entering operation arguments | 210.497 | 23.259 |
| `def8c619` | Expression include cleanup underway | 156.925 | 14.396 |
