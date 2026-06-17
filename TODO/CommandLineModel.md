# Command-Line Model AST

Goal: introduce a structured `CM::Expr<A>` AST for command-line model
expressions, with explicit node annotations and explicit argument-edge
annotations.  Use it beside the current `ptree` pipeline first, then migrate
typechecking, extraction, code generation, and `model_t`.  Keep type
annotations as `ptree` initially; replace them later.

The AST lives in namespace `CmdModel`; use the alias `CM` in signatures and
implementation code when the shorter spelling improves readability.

## Phase 0: Inventory Current Invariants

Before adding converters, document the valid and invalid `ptree` shapes used by
the current command-line model language.

Capture invariants for:

- Literals: int, double, bool, quoted string constants.
- Variables and function names.
- Argument references such as `@x`.
- Function calls with positional and named args.
- Empty positional arguments used during parser/positional-rewrite
  compatibility.
- `List`, `Tuple`, `!let`, `function`, `sample`, `get_state`, and `_`.
- Annotated model fields: `value`, `type`, `used_args`, `is_default_value`,
  `alphabet`, `no_log`, `extract`, `suppress_default`.

Decide which malformed shapes should be rejected by converters instead of
preserved.

Note: some malformed call-like shapes are not distinguishable after they have
been represented as `ptree`.  For example, `ptree("sample", {})` is identical
to the variable `sample`, not a zero-argument `sample` form.  Reject malformed
special forms in converters when the malformed structure is representable, but
handle unrepresentable zero-child special forms at the parser/grammar boundary.

## Phase 1: Define The AST

Create:

- `src/models/model-expr.H`
- `src/models/model-expr.cc`

Core aliases:

```cpp
namespace CmdModel {

using NoAnn = std::monostate;

template<class A>
struct Expr;

template<class A>
struct Arg;

template<class A>
using Decls = std::vector<std::pair<std::string, Expr<A>>>;

using UntypedExpr = Expr<NoAnn>;

}

namespace CM = CmdModel;
```

Use explicit node types:

```cpp
struct IntLiteral { int value; };
struct DoubleLiteral { double value; };
struct BoolLiteral { bool value; };
struct StringLiteral { std::string value; };

struct Var { std::string name; };
struct ArgRef { std::string name; };      // @name
struct Placeholder {};                    // _
struct GetState { std::string state_name; };
struct MissingArg {};                     // compatibility-only, rejected before typechecking
```

Recursive nodes:

```cpp
template<class A>
struct Call {
    std::string function;
    std::vector<Arg<A>> args;
};

template<class A>
struct List {
    std::vector<Expr<A>> elements;
};

template<class A>
struct Tuple {
    std::vector<Expr<A>> elements;
};

template<class A>
struct Let {
    Decls<A> decls;
    Box<Expr<A>> body;
};

template<class A>
struct Lambda {
    Box<Expr<A>> pattern; // later replace with ModelPattern
    Box<Expr<A>> body;
};

template<class A>
struct Sample {
    Box<Expr<A>> dist;
};
```

Argument-edge annotations:

```cpp
template<class A>
struct Arg {
    std::string name; // empty means positional
    Box<Expr<A>> value;

    bool is_default_value = false;
    bool suppress_default = false;

    // Present when this argument has an alphabet expression.
    std::optional<Box<Expr<A>>> alphabet;
};
```

Node annotations:

```cpp
struct Ann {
    ptree type; // temporary; later ModelType or Haskell Type
    std::set<std::string> used_args;

    bool no_log = false;
    std::optional<std::string> extract;
};

using TypedExpr = Expr<Ann>;
using TypedDecls = Decls<Ann>;
```

Expression:

```cpp
template<class A>
struct Expr {
    [[no_unique_address]] A ann;

    using Node = std::variant<
        IntLiteral,
        DoubleLiteral,
        BoolLiteral,
        StringLiteral,
        Var,
        ArgRef,
        Placeholder,
        MissingArg,
        GetState,
        Call<A>,
        List<A>,
        Tuple<A>,
        Let<A>,
        Lambda<A>,
        Sample<A>
    >;

    Node node;
};
```

`CM::Box<T>` should provide non-null owned-tree semantics for required recursive
children.  Prefer unique ownership plus copy support, either by:

- a small local copyable box wrapper around `std::unique_ptr`, or
- explicit clone/copy constructors for recursive fields.

Use `std::optional<CM::Box<T>>` for genuinely optional recursive fields, such
as `CM::Arg::alphabet`.

Avoid `std::shared_ptr` by default.  The model AST is an owned tree, not a
graph, and extraction mutates subtrees.

## Phase 2: Add Copy, Move, And Clone Semantics

Current `ptree` values are copied freely.  Decide early whether `CM::Expr` is
copyable or move-only.

Recommended: make it copyable by defining a local `CM::Box<T>` that deep-copies, or
by adding explicit clone helpers:

```cpp
template<class A>
CM::Expr<A> clone(const CM::Expr<A>&);

template<class A>
CM::Arg<A> clone(const CM::Arg<A>&);

template<class A>
CM::Decls<A> clone(const CM::Decls<A>&);
```

Add tests that copying a tree produces independent subtrees.

## Phase 3: Add Accessors, Visitors, And Invariants

Add helpers so callers do not string-match node names:

```cpp
template<class A>
bool is_call_named(const CM::Expr<A>&, std::string_view);

template<class A>
bool is_list(const CM::Expr<A>&);

template<class A>
bool is_tuple(const CM::Expr<A>&);

template<class A>
bool is_sample(const CM::Expr<A>&);

template<class A>
bool is_get_state(const CM::Expr<A>&);
```

Traversal:

```cpp
template<class A, class F>
void for_each_child(CM::Expr<A>&, F&&);

template<class A, class F>
void for_each_child(const CM::Expr<A>&, F&&);
```

Invariant checks:

```cpp
template<class A>
void check_invariants(const CM::Expr<A>&);

template<class A>
void check_invariants(const CM::Decls<A>&);
```

Initial invariants:

- Tuple has at least two elements.
- `GetState` has exactly one state name and no expression children.
- Let body is non-null by construction.
- Lambda pattern and body are non-null by construction.
- Sample dist is non-null by construction.
- `CM::Arg::alphabet`, if present, is non-null and structurally valid.
- `MissingArg` may appear only in parser/converter compatibility code and must
  be rejected before typechecking or code generation.
- Typed expressions have valid `ann.type` after typechecking.
- Argument-edge metadata lives on `CM::Arg`, not hidden in expression nodes.
- No annotation-only metadata is hidden in expression nodes.

Run strict invariant checks after parser compatibility rewriting, typechecking,
extraction, and before code generation.  Use two boundaries deliberately: raw
conversion may produce compatibility-only nodes such as `MissingArg`, but any
pipeline invariant check after parser compatibility rewriting must reject them.

## Phase 4: Add Compatibility Converters

Put conversion helpers in clearly named compatibility files, for example:

- `src/models/model-expr-ptree.H`
- `src/models/model-expr-ptree.cc`

New code should not casually convert back to `ptree`.  Treat these helpers as a
migration bridge and testing aid.

Add:

```cpp
CM::UntypedExpr model_expr_from_ptree(const ptree&);
ptree ptree_from_model_expr(const CM::UntypedExpr&);

CM::Decls<CM::NoAnn> model_decls_from_ptree(const ptree&);
ptree ptree_from_model_decls(const CM::Decls<CM::NoAnn>&);

CM::TypedExpr typed_model_expr_from_annotated_ptree(const ptree&);
ptree annotated_ptree_from_typed_model_expr(const CM::TypedExpr&);

CM::TypedDecls typed_model_decls_from_annotated_ptree(const ptree&);
ptree annotated_ptree_from_typed_model_decls(const CM::TypedDecls&);
```

Unannotated mapping:

- int -> `IntLiteral`
- double -> `DoubleLiteral`
- bool -> `BoolLiteral`
- quoted string constant -> `StringLiteral`
- unquoted string without children -> `Var`, except:
  - `"_"` -> `Placeholder`
  - strings beginning with `@` -> `ArgRef`
- null/empty positional argument -> `MissingArg`, only if needed for parser
  compatibility
- string with children -> `Call`, except:
  - `"List"` -> `List`
  - `"Tuple"` -> `Tuple`
  - `"!let"` -> `Let`
  - `"function"` -> `Lambda`
  - `"sample"` -> `Sample`
  - `"get_state"` -> `GetState`

`get_state` is currently represented as `ptree("get_state", {{"",
ptree(state_name)}})`: exactly one child, with a string-valued child expression
containing the state name.

Reject malformed `get_state`, malformed `!let`, malformed `function`,
malformed `sample`, and one-element tuples instead of preserving invalid
shapes, when those malformed shapes are representable in `ptree`.  If
`MissingArg` is needed during conversion, it must not survive into the
typechecking or code generation pipeline.

Annotated mapping:

- `ann["value"]` -> expression node
- `ann["type"]` -> `expr.ann.type`
- `ann["used_args"]` -> `expr.ann.used_args`
- `ann["no_log"]` -> `expr.ann.no_log`
- `ann["extract"]` -> `expr.ann.extract`

Argument-edge mapping:

- child key -> `CM::Arg::name`
- child value -> `CM::Arg::value`
- child value's old `"is_default_value"` -> `CM::Arg::is_default_value`
- child value's old `"suppress_default"` -> `CM::Arg::suppress_default`
- child value's old `"alphabet"` -> `CM::Arg::alphabet`

For list and tuple elements, consciously ignore old always-false edge metadata
such as `is_default_value=false`; those are expression children, not function
arguments with defaults.

When converting back to annotated `ptree`, restore the old field layout exactly.

## Phase 5: Add Round-Trip And Invariant Tests

Use the focused Meson test executable `src/model-expr-test` for these tests.
This keeps AST/converter/typechecker/codegen parity checks close to
`src/models` before the main `bali-phy` tests depend on the new representation.

Before changing behavior, add tests for:

- literals
- variables
- arg refs
- calls with positional args
- calls with named args
- lists
- dictionary/list-of-pairs syntax
- tuples
- sample
- let/where
- lambda
- get_state
- placeholder
- missing positional args, if represented
- annotated default argument
- annotated alphabet argument
- no_log/extract fields

Test:

```text
ptree -> CM::Expr -> ptree
annotated ptree -> CM::TypedExpr -> annotated ptree
```

Also test `unparse(...)`, `unparse_annotated(...)`, and generated-code parity
where structural equality is too strict.

When running these tests in this development environment, use the out-of-source
builds under `bali-phy/build/`, for example:

```bash
meson test -C build/gcc-16-debug-O 'model expression AST' --print-errorlogs
```

The configured compiler may use `ccache`; sandboxed test runs can fail if
`ccache` cannot write to its cache.  That is an environment concern, not a test
failure in the project.

## Phase 6: Add Parser Wrappers Without Replacing Parser

Keep existing `parse(...)`, `parse_defs(...)`, and parser grammar unchanged.

Add:

```cpp
CM::UntypedExpr parse_model_expr(const Rules&, const std::string&, const std::string& what);
CM::Decls<CM::NoAnn> parse_model_decls(const Rules&, const std::string&);
```

Initially implement with conversion:

```cpp
return model_expr_from_ptree(parse(R, s, what));
```

This lets new code use the structured AST while old callers still receive
`ptree`.

## Phase 7: Add Untyped Pretty Printing

Add:

```cpp
std::string unparse(const CM::UntypedExpr&);
std::string show_model(const CM::UntypedExpr&);
std::string unparse(const CM::Decls<CM::NoAnn>&);
```

Port behavior from current `unparse(ptree)` but use variants and typed nodes.
Keep existing `ptree` overloads.

## Phase 8: Add Typed Pretty Printing

Add:

```cpp
std::string unparse_annotated(const CM::TypedExpr&);
std::string show_model_annotated(const CM::TypedExpr&);
```

Use:

- `CM::Expr::node` for expression structure.
- `CM::Ann` for node-level flags.
- `CM::Arg` for default/alphabet/suppress metadata.

Keep existing `ptree` overloads.  During transition, they can convert to
`CM::TypedExpr` and delegate.

## Phase 9: Use Untyped AST In New Binding Inference

Use `parse_model_expr(...)` and `CM::UntypedExpr` in the new command-line
binding inference/audit path.

This proves the AST in new code before replacing the existing compile pipeline.

## Phase 10a: Add Compatibility Typechecker Wrappers

Add compatibility typechecker entry points:

```cpp
CM::TypedExpr typecheck_model_expr(
    const TypecheckingState&,
    const ptree& required_type,
    const CM::UntypedExpr&
);

CM::TypedDecls typecheck_model_decls(
    TypecheckingState&,
    const CM::Decls<CM::NoAnn>&
);
```

These wrappers initially convert `CM::UntypedExpr` to `ptree`, call the current
`TypecheckingState::typecheck_and_annotate(...)` implementation, then convert
the annotated `ptree` result back to `CM::TypedExpr`.  Treat this as a bridge
for tests and for future call-site migration, not as the final AST typechecker.

Add parity tests that compare wrapper output against the existing annotated
`ptree` typechecker for literals, variables, declarations, calls/defaults,
alphabet expressions, `let`, lambda, list, tuple, and `get_state` cases.

## Phase 10b: Port Typechecker To Build `CM::TypedExpr` Directly

Port expression typechecking from the old `typecheck_and_annotate_*` functions
one case at a time:

- constants
- variables
- let
- lambda
- tuple
- list
- get_state
- function calls

Current status: `typecheck_model_expr(...)` now has direct AST handlers for
these expression cases and `compile_model(...)` uses it in production.
`compile_model(...)` still converts the typed AST back to annotated `ptree` at
the extraction/code-generation boundary.

The expression typechecker no longer has a ptree bridge fallback.  Conversion
insertion builds AST conversion calls directly for `intToDouble`, `discrete`,
`unit_mixture`, `convertDiscrete`, `multiMixtureModel`, and `f`.  Placeholder
and missing-argument AST nodes now fail with direct errors instead of escaping
to old ptree behavior.

Keep type representation unchanged:

- `CM::Ann::type` is still `ptree`
- `TypecheckingState` still stores `ptree` types
- `equations` still uses `term_t = ptree`

Typed nodes should be built with valid types immediately.  Avoid long-lived
`CM::TypedExpr` values with null `ann.type`; if construction needs staging, use
short-lived local builders rather than partially typed AST nodes.

Replace:

```cpp
get_used_args(ptree)
set_used_args(ptree)
```

with direct access to:

```cpp
expr.ann.used_args
```

For function arguments, set:

```cpp
CM::Arg::is_default_value
CM::Arg::alphabet
CM::Arg::suppress_default
```

Run invariant checks on the result.

The main `compile_model(...)` and `compile_decls(...)` paths both parse and
typecheck through `CmdModel`.  After typechecking, they still convert typed AST
back to annotated `ptree` for extraction, `model_t` display/debug storage, and
code generation.

## Phase 10c: Port Declaration Compile Path

`typecheck_model_decls(...)` is direct and produces `CM::TypedDecls` without
converting through annotated `ptree`.  The production `compile_decls(...)`
path now uses `parse_model_decls(...)` and `typecheck_model_decls(...)`.

The direct declaration path must continue to:

- preserve declaration order and scope extension behavior
- assign fresh type variables for declarations exactly as the old code does
- accumulate `used_args` directly from `CM::Ann::used_args`
- share the same `TypecheckingState::eqs` behavior as the old decl typechecker

Current status: the old per-shape `TypecheckingState::typecheck_and_annotate_*`
handlers have been removed.  `TypecheckingState::typecheck_and_annotate(...)`
remains only as a legacy wrapper: it converts annotated-ptree API requests to
the `CmdModel` AST typechecker, then converts the typed AST result back to
annotated `ptree`.

## Phase 11: Port Substitution Helpers

Add AST overloads:

```cpp
void substitute_annotated(const equations&, CM::TypedExpr&);
void substitute_annotated(const equations&, CM::TypedDecls&);
```

These should mostly recurse through the AST and apply
`substitute(eqs, expr.ann.type)`.

Keep the annotated-`ptree` overload while the current compile path still needs
it.  Remove or quarantine it only after the main annotated-`ptree` path is gone.

## Phase 11b: Port Extraction Helpers

Port extraction logic from `compile.cc`:

- `annotated_term_is_model`
- `bound`
- `do_extract`
- `extract_terms`
- `pretty_model_t`

Use `CM::TypedExpr` and `CM::Arg` metadata.

Be careful: extraction mutates the tree.  Use value ownership and `clone(...)`
where needed.  Avoid shared subtrees.

Add tests for extracted pretty output.

## Phase 12: Move The Codegen Boundary To Typed AST

After typechecking, production code should keep using:

```cpp
CM::TypedExpr
CM::TypedDecls
```

instead of converting back to annotated `ptree` before code generation.  Adding
an AST overload is acceptable only if the next implementation step starts making
that overload native.  Do not let whole-function wrappers become a long-lived
parallel pipeline.

Add `CodeGenState` overloads with the same return types as the existing
annotated-ptree entry points:

```cpp
CodeGenState::get_model_as(const CM::TypedExpr&);
CodeGenState::get_model_decls(const CM::TypedDecls&);
```

The first implementation may be a temporary compatibility wrapper:

```cpp
// Compatibility boundary: codegen still consumes annotated ptree internally.
// Remove once this overload is implemented directly over CM::TypedExpr.
return get_model_as(CM::annotated_ptree_from_typed_model_expr(model));
```

and similarly for declarations.  Immediately switch production compile callers:

- `compile_model(...)` should call `get_model_as(typed_model)`.
- `compile_decls(...)` should call `get_model_decls(typed_decls)`.

`model_t` may still store annotated `ptree` for display/debugging in this phase.
That conversion should be separate from the value passed to code generation.

Run after this boundary move:

- `bali-phy:model expression AST`
- `bali-phy:runtime AST serialization`
- `bali-phy:bali-phy 5d +A 50`
- full `tests/parse`

## Phase 13: Make Declaration Codegen Native First

Make `get_model_decls(const CM::TypedDecls&)` native for declaration traversal.
It should iterate directly over:

```cpp
for (auto& [name, expr] : decls)
    ...
```

instead of converting the declaration block to `ptree("!Decls", ...)`.

If expression bodies still need old expression codegen temporarily, convert only
the individual declaration expression at the leaf:

```cpp
auto expr_ptree = CM::annotated_ptree_from_typed_model_expr(expr);
```

This is still progress because the declaration container and production
`compile_decls(...)` path no longer depend on `!Decls`.  Add a brief
compatibility note at each local expression fallback, explaining that it exists
only until expression codegen is AST-native.

After this step, delete unused helpers that exist only to traverse annotated
`!Decls`.

## Phase 14: Make Expression Codegen Dispatch Native

Make `get_model_as(const CM::TypedExpr&)` native at the dispatch level.  It
should switch on:

```cpp
expr.node
expr.ann
```

Temporary fallback is allowed only per unconverted variant, not as a
whole-expression unconditional conversion.  For example:

```cpp
return std::visit(overloaded{
    [&](const CM::IntLiteral&) { return get_constant_model(expr); },
    [&](const CM::Call<CM::Ann>&) {
        // Compatibility boundary: calls still use annotated ptree codegen.
        // Remove after call codegen reads CM::Call and CM::Arg directly.
        return get_model_as(CM::annotated_ptree_from_typed_model_expr(expr));
    },
    ...
}, expr.node);
```

Each fallback branch must be:

- local to a specific unconverted variant
- commented as compatibility
- deleted as soon as that variant is converted

Audit after every batch:

- Which production paths still call annotated-ptree codegen?
- Which AST overload branches still fallback to annotated `ptree`?
- Which annotated-ptree helpers are now unused and can be deleted?

## Phase 15: Convert Expression Codegen Variants

Convert variants in small batches.  Prefer variants with little codegen-specific
metadata first:

1. Literals, variables, and `get_state`.
2. `List` and `Tuple`.
3. `Let`, reusing native `CM::TypedDecls` declaration codegen.
4. Ordinary rule-backed calls.
5. `Sample` and `Lambda`, if they need special handling.

For calls, read directly from:

```cpp
CM::Call<CM::Ann>
CM::Arg<CM::Ann>
```

Preserve existing behavior for:

- default arguments
- suppressed defaults
- alphabet expressions
- extracted values
- `no_log`
- `used_args`
- random/action-producing arguments
- logging decisions

Replace old accesses:

```cpp
model.get_child("value")
arg.get_child("type")
arg.get_child("is_default_value")
arg.get_child_optional("alphabet")
```

with direct AST access:

```cpp
expr.node
expr.ann.type
arg.value
arg.is_default_value
arg.suppress_default
arg.alphabet
```

After each variant batch, run focused model/codegen tests plus at least the
`5d +A 50` test for meaningful production coverage.

## Phase 16: Invert Or Remove Ptree Codegen Compatibility

Once AST codegen is native, old annotated-ptree overloads should either be
deleted or inverted into wrappers:

```cpp
// Compatibility boundary: old callers still pass annotated ptree.
// Remove once all callers use CM::TypedExpr.
return get_model_as(CM::typed_model_expr_from_annotated_ptree(model));
```

Production compile paths should use only:

```cpp
CM::TypedExpr
CM::TypedDecls
```

Keep annotated-ptree conversion only for:

- display/debug output
- tests that intentionally exercise conversion
- short-lived compatibility wrappers with explicit comments

## Phase 17: Update `model_t` And `pretty_model_t`

Change model storage from annotated `ptree` to typed AST:

```cpp
class model_t {
    CM::TypedExpr description;
    ...
};
```

For declarations, either:

- introduce a separate declaration model type, or
- use `std::variant<CM::TypedExpr, CM::TypedDecls>` if one class must store
  both.

Update:

- `show`
- `show_pretty`
- `show_main`
- `show_extracted`
- JSON pretty conversion

## Phase 18: Remove Annotated-`ptree` Main Path

Once main compilation uses `CM::TypedExpr`, remove or quarantine:

- annotated-ptree codegen overloads
- annotated-ptree extraction helpers
- `substitute_annotated(ptree&)`
- `get_used_args(ptree)`
- `set_used_args(ptree&)`

Keep conversion helpers only for tests/debugging until direct parser output is
migrated.

## Phase 19: Replace Annotation Type Later

After the AST migration is stable, replace:

```cpp
ptree CM::Ann::type;
```

with:

```cpp
ModelType CM::Ann::type;
```

or Haskell `Type`.

This should be much smaller because type annotations are now explicit fields.

## Phase 20: Introduce A Real Pattern AST Later

`Lambda::pattern` can initially use `CM::Expr<A>` for compatibility, but
patterns are not arbitrary expressions.  Later introduce:

```cpp
template<class A>
struct ModelPattern;
```

or an unannotated `ModelPattern` if pattern annotations are not useful.

Current patterns support variables and tuple/list-like structures.  Moving them
out of `CM::Expr` will simplify lambda validation and typechecking.

## Phase 21: Make Parser Build `CM::Expr` Directly Later

Once converters are no longer central, update `parser.y` semantic values to
build:

- `CM::UntypedExpr`
- `CM::Decls<CM::NoAnn>`

directly instead of `ptree`.

This removes old expression encodings and conversion overhead.

## Recommended Milestone Order

1. Inventory current invariants.
2. AST, box/copy semantics, accessors, invariants.
3. Compatibility converters and round-trip tests.
4. Parser wrappers.
5. Untyped and typed pretty-printing.
6. Use untyped AST in new binding inference/audit code.
7. Compatibility typechecker wrappers and parity tests.
8. Expression typechecker builds `CM::TypedExpr` directly.
9. Declaration typechecker builds `CM::TypedDecls` directly.
10. Remove production reliance on expression typechecker compatibility.
11. AST substitution overloads.
12. Extraction helpers consume `CM::TypedExpr`.
13. Move the codegen production boundary to `CM::TypedExpr` / `CM::TypedDecls`.
14. Make declaration codegen native over `CM::TypedDecls`.
15. Make expression codegen dispatch native over `CM::TypedExpr`.
16. Convert expression codegen variants and delete fallback branches.
17. Invert or remove annotated-ptree codegen compatibility.
18. Update `model_t` and `pretty_model_t` to store/consume typed AST.
19. Remove annotated-`ptree` main path.
20. Replace `CM::Ann::type`.
21. Introduce a real pattern AST.
22. Make parser direct.

## Risk Notes

- Code generation and extraction are highest risk.
- Adding AST overloads should move production call sites immediately; native
  overload bodies should follow before adding more compatibility infrastructure.
- Keep `CM::Ann::type = ptree` until the AST migration is complete.
- Keep parser output unchanged initially.
- Model argument metadata should live on `CM::Arg`, not in node annotations.
- Avoid `shared_ptr` for tree ownership unless copyability proves too costly.
- Quarantine compatibility converters so the migration does not stall with both
  representations everywhere.
