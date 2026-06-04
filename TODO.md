# TODO

## Runtime evaluation cleanup

- Rename temporary runtime-returning APIs that end in `_code` once the legacy
  `expression_ref` APIs are removed. The intended long-term names are the
  undecorated API names.
- Remove the remaining legacy evaluation APIs from `context_ref` and
  `reg_heap`, including `evaluate_*`, `get_reg_value*`, and
  `evaluate_program` overloads that return `expression_ref`.
- Finish converting remaining `context_ptr` callers in tree, parameter, MCMC,
  and SMC code to `value_code()`, `head_code()`, `set_code()`, and
  `list_to_vector_code()`. The SMC list consumers are the largest remaining
  caller cluster.
- Remove non-evaluator `reg_heap::expression_at()` call sites such as
  interchangeables and graph-register comparison/debug helpers.
- Keep `Runtime::to_expression_ref()` and `closure::legacy_exp()` only at
  parser/model-generation/debug-display boundaries.
