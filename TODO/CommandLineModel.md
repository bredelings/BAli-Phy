# Command-Line Model AST Migration

Goal: make command-line model expressions use the structured `CmdModel`
(`CM`) AST through parsing wrappers, typechecking, extraction, code generation,
and model storage.  Keep model types as `ptree` for now; this plan is about
removing `ptree` as the expression representation.

The parser grammar still builds `ptree`.  That is intentional for now: the
current migration boundary is immediately after parsing, where parser wrappers
convert to `CM::UntypedExpr` or `CM::Decls<CM::NoAnn>`.

## Current State

Implemented infrastructure:

- `src/models/model-expr.H` defines `CM::Expr<A>`, `CM::Arg<A>`,
  `CM::Decls<A>`, `CM::Ann`, and `CM::Box<T>`.
- `src/models/model-expr-ptree.H/cc` contain compatibility converters between
  legacy model-expression `ptree` shapes and `CM`.
- `src/models/parse.cc` has untyped and typed `CM` pretty/unparse support.
- `src/models/model-expr-test.cc` covers converter round trips, malformed
  shape rejection, AST typechecking parity, and substitution behavior.

Production paths already using `CM`:

- `compile_model(...)` parses through `parse_model_expr(...)`, typechecks with
  `typecheck_model_expr(...)`, extracts over `CM::TypedExpr`, and calls
  codegen over `CM::TypedExpr`.
- `compile_decls(...)` parses through `parse_model_decls(...)`, typechecks with
  `typecheck_model_decls(...)`, extracts over `CM::TypedDecls`, and calls
  declaration codegen over `CM::TypedDecls`.
- `model_t` stores typed AST descriptions as either `CM::TypedExpr` or
  `CM::TypedDecls`.
- Pretty/extraction storage uses typed AST, not annotated `ptree`.
- Code generation dispatches over `CM::TypedExpr` and `CM::TypedDecls`.
- Rule-backed call codegen reads `CM::Call<CM::Ann>` and `CM::Arg<CM::Ann>`
  directly, including defaults, alphabets, logging, and computed values.
- Random/logging predicates operate on `CM::TypedExpr`.
- `RuleArg::default_value`, `RuleArg::alphabet`, `Rule::call`, and
  `ComputedRule::value` all store `CM::UntypedExpr`.

Recent migration milestones:

- Removed the old expression-typechecker fallback to annotated `ptree`.
- Removed old per-shape `typecheck_and_annotate_*` handlers.
- Removed `model_t` annotated-`ptree` storage.
- Removed the `RuleTemplateExpr` split.
- Removed old `make_call(const ptree&, ...)` rule-template codegen.

## Remaining `ptree` Roles

These uses are expected to remain for now:

- Parser implementation and generated parser files.
- Type syntax and unification:
  - `CM::Ann::type`
  - `TypecheckingState` type maps
  - `term_t`
  - rule result types, argument types, and constraints
- Raw JSON loading of binding files into `ptree`.
- Help-topic files and unrelated help-tree traversal.
- Conversion tests that intentionally exercise `ptree` round trips.

These uses should shrink next:

- Compatibility wrappers that accept or return annotated model-expression
  `ptree`.
- Back-conversions from `CM::Expr` to `ptree` inside typechecking.
- Raw parser entry points used outside parser compatibility wrappers.

## Temporary Compatibility Notes

Keep these comments near the relevant code until the behavior is removed or
unified:

- Rule templates still share the model parser.  Because of this,
  `sample(@dist)` parses as `CM::Sample`, but rule-template codegen treats it as
  a Haskell `sample` call.
- Rule templates encode `submodel +> f` as a final `@submodel` argument.  Codegen
  preserves this as a compatibility behavior until bindings spell the operation
  explicitly.
- Rule model/default/alphabet/template strings are still parsed by the legacy
  parser and converted immediately to `CM` at rule-load time.  This boundary can
  disappear once the parser builds `CM` directly.

## Next Implementation Batch

This is the next recommended chunk.  It should make real progress by removing
production expression back-conversions instead of adding parallel paths.

1. Audit annotated-`ptree` wrappers.

   Search for:

   ```text
   typecheck_and_annotate
   substitute_annotated(ptree
   get_used_args(ptree
   set_used_args(ptree
   typed_model_expr_from_annotated_ptree
   annotated_ptree_from_typed_model_expr
   ```

   For each occurrence, classify it as:

   - required parser/converter/test compatibility
   - obsolete production wrapper
   - old API wrapper that can be inverted to call `CM` code

2. Delete or invert obsolete annotated-`ptree` wrappers.

   If a wrapper has no production caller, delete it.  If a wrapper exists only
   for old callers, keep it as a narrow compatibility wrapper around `CM` and
   add a brief comment explaining who still calls it and what removes it.

3. Remove `ptree_from_model_expr(...)` from call typechecking.

   `typecheck_model_call(...)` should not convert the current expression back to
   `ptree` just to produce error text or drive old helper logic.  Replace those
   uses with direct `CM` handling or `unparse(expr)`.

4. Make lambda pattern parsing native.

   `typecheck_model_lambda(...)` currently converts the lambda pattern to
   `ptree` and calls `parse_pattern(const ptree&)`.  Add a direct
   `parse_pattern(const CM::UntypedExpr&)` helper, or a narrow pattern helper,
   that supports the same pattern forms currently accepted:

   - variables
   - tuple patterns

   Reject unsupported expression nodes with clear errors.

5. Run focused tests after the batch.

   Required:

   ```bash
   timeout 120s meson test -C ../build/gcc-16-debug-O "bali-phy:model expression AST" --print-errorlogs
   timeout 120s meson test -C ../build/gcc-16-debug-O "bali-phy:runtime AST serialization" --print-errorlogs
   timeout 240s meson test -C ../build/gcc-16-debug-O "bali-phy:bali-phy 5d +A 50" --print-errorlogs
   timeout 300s tests/run-tests.py run tests/parse /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O/src/bali-phy/bali-phy --package-path=/home/bredelings/Devel/bali-phy/build/gcc-16-debug-O/src/builtins:/home/bredelings/Devel/bali-phy/jj --seed 1594303352
   ```

6. Commit as one statement.

   Suggested message:

   ```text
   Remove typechecking ptree expression fallbacks
   ```

## Following Batch

After the immediate batch, tighten parser and rule boundaries.

1. Add AST overloads for rule result-type queries.

   Replace expression-shaped APIs such as:

   ```cpp
   Rules::get_result_type(const ptree& model_rep)
   ```

   with direct `CM` logic, or remove the API if typechecking can compute the
   result directly.

2. Restrict raw parser entry points.

   Higher-level model code should consume:

   ```cpp
   CM::UntypedExpr parse_model_expr(const Rules&, const std::string&, const std::string&);
   CM::Decls<CM::NoAnn> parse_model_decls(const Rules&, const std::string&);
   CM::UntypedExpr parse_rule_template_expr(...);
   ```

   Keep `parse_expression(...)` and `parse_defs(...)` localized to the parser
   compatibility layer, rule loading, and converter tests.

3. Audit remaining `model_expr_from_ptree(...)` and `ptree_from_model_expr(...)`.

   Acceptable remaining uses should be only:

   - parser wrappers
   - rule-load conversion boundary
   - conversion tests
   - old compatibility API wrappers with comments

## Later Work

### Direct Parser Output

Once semantic stages no longer need expression-shaped `ptree`, update
`parser.y` semantic values to build:

```cpp
CM::UntypedExpr
CM::Decls<CM::NoAnn>
```

directly.  This should remove most `ptree` expression encodings and reduce the
importance of `model-expr-ptree.cc`.

### Real Pattern AST

`CM::Lambda::pattern` currently uses `CM::Expr<A>` for compatibility, but
patterns are not arbitrary expressions.  Introduce a small pattern AST after
native pattern handling is stable.

Likely initial forms:

- variable pattern
- tuple pattern

### Replace Type Representation

After expression migration is stable, replace type `ptree` values:

```cpp
ptree CM::Ann::type;
```

with a real type representation, either a local `ModelType` or the existing
Haskell type representation.  Do this after expression migration so type changes
do not obscure expression-representation changes.

### Retire Conversion Helpers

Eventually keep `model-expr-ptree` only for tests/debugging, or delete it
entirely if parser output and compatibility APIs no longer need it.

## Risk Notes

- Type syntax is still `ptree`; do not mix expression migration with type
  representation migration unless a change specifically requires it.
- Rule templates are Haskell-ish expressions parsed by the model parser, so they
  need explicit compatibility notes when parser semantics differ from template
  semantics.
- Extraction mutates ASTs; keep value ownership clear and avoid shared
  subtrees.
- When adding temporary wrappers or workarounds, add a brief comment explaining
  why they exist, what they do, and how to remove them.
- For each new function or lambda longer than three lines, add a 1-2 line
  comment immediately before it.
