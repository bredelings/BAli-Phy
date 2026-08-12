# `dirichletProcessMap` benchmark

This benchmark separates fixed-domain construction, equal-sized domain
replacement, and variable-dimensional rebuilding.  The performance scenarios
use `delta 0` so parent and map machinery dominate; `trace` uses `normal 0 1`
and logs the complete map for semantic comparison.

Run from the source checkout with a GCC 16 release build:

```sh
ninja -C ../build/gcc-16

env BALIPHY_DP_MAP_SCENARIO=fixed BALIPHY_DP_MAP_SIZE=8 \
  ../build/gcc-16/src/bali-phy/bali-phy \
  run benchmarks/dirichlet-process-map/Model.hs \
  --package-path=../build/gcc-16/src/builtins:. \
  --seed=1 -- --iterations=1 --name=dp-map-smoke
```

The remaining scenarios are `changing-keys`, `changing-size`, and `trace`.
Use sizes 256 and 1024 for performance measurements and size 32 for trace
comparison.

For each performance scenario, size, and seed 1 through 7, measure one and
fifty iterations from a fresh temporary output directory:

```sh
perf stat -x '\t' -e instructions,cycles -- \
  env BALIPHY_DP_MAP_SCENARIO=changing-keys BALIPHY_DP_MAP_SIZE=1024 \
  "$PROJECT/build/gcc-16/src/bali-phy/bali-phy" \
  run "$PROJECT/jj/benchmarks/dirichlet-process-map/Model.hs" \
  --package-path="$PROJECT/build/gcc-16/src/builtins:$PROJECT/jj" \
  --seed=1 -- --iterations=50 --name=run
```

Estimate recurring work within each run as `(I50-I1)/49`.  Interleave the
baseline and candidate revisions, reversing their order for every other seed,
and warm the compiled-module cache once after each rebuild.  Instructions are
the primary metric; cycles are supporting evidence.

For every seed, run `trace 32` for fifty iterations and record the exact
`run-1/C1.log.json` hash.  Compare the header and iteration-zero row exactly
to verify initial clustering, key/value alignment, and random-number
consumption.  Full MCMC hashes are comparable only when a change preserves
runtime graph traversal: changing the graph can reorder valid moves and hence
their random draws without changing the target distribution.  In that case,
use the MCMC posterior tests for distributional validation and retain both
full hashes in the results.  The delta cases verify only completion and domain
size because all returned values are identical.

Record each invocation in `runs.tsv` with run order, revision, compiler, host,
scenario, size, seed, iteration count, instructions, cycles, elapsed time,
trace hash, and cache status.  Summarize the paired medians and retention
decision in `results.md` after both revisions have been measured.
