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

Port existing `typecheck_and_annotate_*` functions one at a time:

- constants
- variables
- let
- lambda
- tuple
- list
- get_state
- function calls

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

Do not switch the main compile path to `typecheck_model_expr(...)` merely
because the wrapper exists.  The compile path should wait until
`typecheck_model_expr(...)` is a direct AST implementation and extraction and
code generation have direct AST paths too.

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

## Phase 12: Port Code Generation To Consume `CM::TypedExpr`

Add typed overloads in `CodeGenState`:

```cpp
translation_result_t get_model_as(const CM::TypedExpr&) const;
translation_result_t get_model_decls(const CM::TypedDecls&);
```

Port:

- `is_random`
- `is_unlogged_random`
- `should_log`
- `get_constant_model`
- `get_variable_model`
- `get_model_let`
- `get_model_lambda`
- `get_model_list`
- `get_model_tuple`
- `get_model_state`
- `get_model_function`

Replace old accesses:

```cpp
model.get_child("value")
arg.get_child("type")
arg.get_child("is_default_value")
arg.get_child_optional("alphabet")
```

with:

```cpp
model.node
arg.value.ann.type
arg.is_default_value
arg.alphabet
```

Add representative generated-code golden tests, or at least compare
`generated_code_t::print()` before and after for models involving:

- defaults
- random arguments
- alphabet expressions
- logging
- let expressions
- lambda expressions
- submodels

Do not implement the typed code generation path by converting
`CM::TypedExpr` back to annotated `ptree` and calling the old codegen, except
as a temporary test oracle.  The typed overloads should be direct
implementations so semantic drift is visible during parity testing.

## Phase 13: Switch Compile Path

Change `compile_model(...)`:

1. `parse_model_expr(...)`
2. `typecheck_model_expr(...)`
3. `substitute_annotated(...)`
4. code generation from `CM::TypedExpr`
5. return `model_t` storing `CM::TypedExpr`

Change `compile_decls(...)` similarly using `CM::Decls`.

Keep compatibility conversion only where an old API still needs annotated
`ptree`.

Do not use bridge implementations that convert `CM::TypedExpr` back to
annotated `ptree` as the production path for this phase.

## Phase 14: Update `model_t` And `pretty_model_t`

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

## Phase 15: Remove Annotated-`ptree` Main Path

Once main compilation uses `CM::TypedExpr`, remove or quarantine:

- annotated-ptree codegen overloads
- annotated-ptree extraction helpers
- `substitute_annotated(ptree&)`
- `get_used_args(ptree)`
- `set_used_args(ptree&)`

Keep conversion helpers only for tests/debugging until direct parser output is
migrated.

## Phase 16: Replace Annotation Type Later

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

## Phase 17: Introduce A Real Pattern AST Later

`Lambda::pattern` can initially use `CM::Expr<A>` for compatibility, but
patterns are not arbitrary expressions.  Later introduce:

```cpp
template<class A>
struct ModelPattern;
```

or an unannotated `ModelPattern` if pattern annotations are not useful.

Current patterns support variables and tuple/list-like structures.  Moving them
out of `CM::Expr` will simplify lambda validation and typechecking.

## Phase 18: Make Parser Build `CM::Expr` Directly Later

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
8. Typechecker builds `CM::TypedExpr` directly.
9. AST substitution overloads.
10. Extraction helpers consume `CM::TypedExpr`.
11. Code generation consumes `CM::TypedExpr`.
12. Switch compile path and `model_t`.
13. Remove annotated-`ptree` main path.
14. Replace `CM::Ann::type`.
15. Introduce a real pattern AST.
16. Make parser direct.

## Risk Notes

- Code generation and extraction are highest risk.
- Keep `CM::Ann::type = ptree` until the AST migration is complete.
- Keep parser output unchanged initially.
- Model argument metadata should live on `CM::Arg`, not in node annotations.
- Avoid `shared_ptr` for tree ownership unless copyability proves too costly.
- Quarantine compatibility converters so the migration does not stall with both
  representations everywhere.
