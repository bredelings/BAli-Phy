# TODO

## Runtime evaluation cleanup

- Convert remaining SMC haplotype/panel/site list consumers from `EVector` /
  `expression_ref` helpers to `Runtime::RVector` in focused groups.
- Keep any future bridge from runtime values back to `expression_ref` narrow,
  locally named as legacy/debug/model-generation plumbing, and avoid recreating
  broad shims like the removed `Runtime::to_expression_ref()` or
  `closure::legacy_exp()`.
- Keep `deindexify`, `unlet`, `untranslate_vars`, `subst_reg_vars`, and
  trim/graph unnormalization names aligned with their normalization inverses.
  Do not reuse these names for runtime closure-environment resolution.
- If diagnostics need a runtime-native display pipeline later, first define a
  true runtime-to-Core translation story instead of cloning expression-level
  display transforms under the same names.
