# TODO

## Runtime evaluation cleanup

- Rename temporary runtime-returning APIs that end in `_code` once the legacy
  `expression_ref` APIs are removed. The intended long-term names are the
  undecorated API names.
- Finish converting remaining `context_ptr` callers in tree, parameter, MCMC,
  and SMC code to `value_code()`, `head_code()`, `set_code()`, and
  `list_to_vector_code()`. The SMC haplotype/panel/site list consumers are the
  largest remaining caller cluster; haplotype-index lists are already runtime
  lists.
- Keep `Runtime::to_expression_ref()` and `closure::legacy_exp()` only at
  parser/model-generation/debug-display boundaries.
- Keep `deindexify`, `unlet`, `untranslate_vars`, `subst_reg_vars`, and
  trim/graph unnormalization names aligned with their normalization inverses.
  Do not reuse these names for runtime closure-environment resolution.
- If diagnostics need a runtime-native display pipeline later, first define a
  true runtime-to-Core translation story instead of cloning expression-level
  display transforms under the same names.
