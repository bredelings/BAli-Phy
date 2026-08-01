# `dirichletProcessMap` benchmark results

## Environment and method

Measurements ran on `centromere` with `g++-16 (Debian 16.1.0-3) 16.1.0`
and the `build/gcc-16` release configuration.  The measured baseline was
`5f47f70b61e8`; the measured candidate was `0885a4e94eb3`.  The final stack
contains source-equivalent revisions `11b032ad` and `18a392aa`, respectively:
the later edits only corrected the benchmark documentation and expanded a test.

Each table entry summarizes seven fixed seeds.  Recurring work is
`(counter50 - counter1) / 49`.  “Median ratio” compares the two revision
medians, while “paired change” is the median of the seven same-seed percentage
changes.  Retired instructions are the decision metric; cycles are supporting
evidence.  Cache warm-up runs were setup and are not included in `runs.tsv`.
The recorded measurements were taken in baseline and candidate phases rather
than interleaved; `runs.tsv` preserves their actual order.

## One-iteration measurements

| Scenario | Keys | Baseline instructions | Candidate instructions | Median ratio | Paired change |
| --- | ---: | ---: | ---: | ---: | ---: |
| fixed | 256 | 16,024,276,144 | 16,011,571,635 | -0.08% | -0.06% |
| fixed | 1024 | 16,980,990,237 | 16,986,955,903 | +0.04% | +0.04% |
| changing-keys | 256 | 16,153,928,036 | 16,143,143,836 | -0.07% | -0.07% |
| changing-keys | 1024 | 17,678,436,053 | 17,502,956,017 | -0.99% | -1.02% |
| changing-size | 256 | 16,193,284,763 | 16,185,195,706 | -0.05% | -0.10% |
| changing-size | 1024 | 17,902,556,518 | 17,697,206,671 | -1.15% | -1.14% |

## Estimated recurring measurements

| Scenario | Keys | Baseline instructions | Candidate instructions | Instruction median ratio | Instruction paired change | Cycle median ratio | Cycle paired change |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| fixed | 256 | 12,545,214 | 13,096,903 | +4.40% | +1.64% | -17.63% | -15.48% |
| fixed | 1024 | 75,823,338 | 76,041,450 | +0.29% | +0.16% | -2.26% | -2.26% |
| changing-keys | 256 | 38,327,340 | 37,603,121 | -1.89% | -3.50% | -3.18% | -0.57% |
| changing-keys | 1024 | 221,156,662 | 214,516,436 | -3.00% | -2.05% | -6.70% | -5.38% |
| changing-size | 256 | 81,001,208 | 74,123,334 | -8.49% | -8.21% | -12.23% | -14.96% |
| changing-size | 1024 | 435,929,685 | 405,915,845 | -6.89% | -6.70% | -9.97% | -10.63% |

## Retention decision

Retain the vector-to-`IntMap` path.  Changing-cardinality recurring
instructions fall by 8.49% at 256 keys and 6.89% at 1024 keys, comfortably
clearing the 3% decision gate.  The 1024-key equal-cardinality case also falls
by 3.00% by the revision medians.  Cycles support these improvements, falling
by 6.70% to 12.23% in the large or changing-cardinality cases.

The fixed 256-key recurring estimate has a 4.40% ratio-of-medians increase, but
this is not a reproducible regression: same-seed changes have mixed signs and
a paired median of +1.64%, below the 2% limit, while cycles improve.  Fixed
one-iteration construction is unchanged within 0.1% at both sizes.  No other
instruction case regresses.

Do not add native `IntSet` sorting now.  The end-to-end changing-domain cases
already meet the retention gate, so the conditional profiling requirement for
that additional infrastructure was not reached.

## Semantic trace comparison

For every seed from 1 through 7, the JSON header and complete iteration-zero
row are byte-identical between revisions.  This verifies identical initial
cluster assignments, atom values, key/value alignment, and initial random draw
consumption.  The full 50-iteration hashes differ and are retained in
`runs.tsv`: removing the recursive `IntMap.fromList` graph changes valid MCMC
move traversal and therefore which move consumes a later random draw first.
The focused analytic DP MCMC posterior test is used to validate the unchanged
target distribution.

