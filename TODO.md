# TODO

## Runtime evaluation cleanup

- Rename temporary runtime-returning APIs that end in `_code` once the legacy
  `expression_ref` APIs are removed. The intended long-term names are the
  undecorated API names.
- Remove `reg_heap::expression_at()` from evaluator internals. It currently
  routes through `closure::legacy_exp()` and should be replaced by direct
  `Runtime::Exp` inspection.
- Convert `context_ptr` value/head/list helpers to runtime values so tree,
  parameter, MCMC, and SMC code stop depending on evaluated `expression_ref`
  results.
- Keep `Runtime::to_expression_ref()` and `closure::legacy_exp()` only at
  parser/model-generation/debug-display boundaries.
