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
- Removed obsolete annotated-`ptree` typechecking wrappers and expression
  back-conversions from typechecking.
- Removed the unused expression-shaped `Rules::get_result_type(const ptree&)`
  API.

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

- Raw parser entry points used outside parser compatibility wrappers.
- Rule-load conversions from parser `ptree` to `CM`.
- General converter use outside tests, parser wrappers, and explicitly marked
  compatibility boundaries.

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

The next recommended chunk is to prepare for a direct parser output by making
the remaining parser compatibility boundaries explicit and narrow.

1. Audit raw parser entry points.

   Search for:

   ```text
   parse_expression(
   parse_defs(
   parse(const Rules&
   parse_defs(const Rules&
   model_expr_from_ptree(
   ptree_from_model_expr(
   ```

   Acceptable production uses should be only:

   - parser wrappers in `parse.cc`
   - rule-load conversion helpers in `rules.cc`
   - converter implementation
   - converter tests

2. Replace or quarantine raw parser callers.

   Higher-level model code should consume:

   ```cpp
   CM::UntypedExpr parse_model_expr(const Rules&, const std::string&, const std::string&);
   CM::Decls<CM::NoAnn> parse_model_decls(const Rules&, const std::string&);
   ```

   Keep raw `ptree` parse helpers only at compatibility boundaries, with brief
   comments saying they can be removed once `parser.y` builds `CM` directly.

3. Reduce rule-load parser conversions.

   `rules.cc` currently has two intentional conversion helpers:

   - `parse_rule_model_expr(...)`: model-language expression plus positional
     rewriting.
   - `parse_rule_template_expr(...)`: Haskell-ish rule template parsed by the
     model parser.

   Keep both for now, but audit whether either can call a `CM` parser wrapper
   instead of open-coding `model_expr_from_ptree(...)`.  Do not merge them unless
   the semantic difference is removed.

4. Run focused tests after the batch.

   Required:

   ```bash
   timeout 120s meson test -C ../build/gcc-16-debug-O "bali-phy:model expression AST" --print-errorlogs
   timeout 120s meson test -C ../build/gcc-16-debug-O "bali-phy:runtime AST serialization" --print-errorlogs
   timeout 240s meson test -C ../build/gcc-16-debug-O "bali-phy:bali-phy 5d +A 50" --print-errorlogs
   timeout 300s tests/run-tests.py run tests/parse /home/bredelings/Devel/bali-phy/build/gcc-16-debug-O/src/bali-phy/bali-phy --package-path=/home/bredelings/Devel/bali-phy/build/gcc-16-debug-O/src/builtins:/home/bredelings/Devel/bali-phy/jj --seed 1594303352
   ```

5. Commit as one statement.

   Suggested message:

   ```text
   Tighten model parser ptree boundaries
   ```

## Following Batch

After parser boundaries are narrow, choose between these two larger steps:

1. Make parser output direct `CM`.

   Update `parser.y` semantic values to build `CM::UntypedExpr` and
   `CM::Decls<CM::NoAnn>` directly.  Keep type parsing as `ptree`.

2. Introduce a real pattern AST.

   `CM::Lambda::pattern` still uses `CM::Expr<A>`.  Native lambda pattern
   parsing is in place, so a small pattern AST can now replace expression-shaped
   patterns without dragging old `ptree` behavior along.

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
