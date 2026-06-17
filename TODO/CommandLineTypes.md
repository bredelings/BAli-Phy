# Command-Line Model Types

Goal: keep command-line model type terms represented as native `CM::Type`
values, not `ptree`.  Haskell type inference is out of scope for this plan; see
`CommandLineHaskellTypes.md` for that later work.

## Current State

The production command-line model path now uses `CM::Type` for model types:

- `src/models/model-type.H/cc` defines the native type AST:
  - `NoType`
  - `TypeCon{name}`
  - `TypeVar{name}`
  - `TypeApp{head, arg}`
- Recursive `TypeApp` nodes use the shared `CM::Box<T>` storage from
  `src/models/model-box.H`.
- `term_t` and `type_t` are aliases for `CM::Type`.
- `Rule::result_type`, `RuleArg::type`, and `Rule::constraints` are native
  types after JSON binding loading.
- `CM::Ann::type` is native.
- `TypecheckingState`, unification, model typechecking, code generation,
  `model_t`, and the A/T model construction helpers use native types.
- The command-line type grammar in `parser.y` returns `CM::Type` directly, and
  `parse_type(...)` returns `CM::Type`.
- There is no production `ptree -> CM::Type` converter.

`ptree` remains in nearby code for unrelated model-expression compatibility
paths, raw JSON rule loading, citations/docs, and older annotated-ptree display
helpers.

## Migration Rules

- Do not reintroduce long-lived `ptree` type bridges.
- New tests should exercise production APIs that already use `CM::Type`; avoid
  converter-only tests.
- Temporary compatibility helpers must be commented with what they preserve and
  when they can be removed.
- Prefer deleting obsolete type infrastructure over retaining escape hatches.

## Native Type API

The native API currently provides:

- `type_con("Int")`
- `type_var("a")`
- `type_atom("a")`, which preserves the old lowercase-name convention for type
  variables
- `type_app(head, arg)`
- `type_apps(head, args)`
- `get_type_apps(type)`
- `get_type_head(type)`
- `list_type(t)`
- `tuple_type({a,b,...})`
- `function_type(a,b)`
- `find_variables_in_type(type)`
- `unparse_type(type)`

`CM::Type` has stable equality and ordering for `set`, `map`, unification
records, and constraints.

## Remaining Work

1. Audit remaining `ptree` references in `src/models` and classify them as:
   unrelated expression/JSON/documentation uses, removable commented code, or
   stale type remnants.
2. Consider whether the global `make_type_app(...)` / `make_type_apps(...)`
   compatibility names should be renamed or moved into `CM` once call sites are
   easy to update.  They are native helpers now, not ptree bridges.
3. Revisit old annotated-ptree display helpers in `parse.cc`.  If no production
   path uses them, delete them and the temporary `unparse_ptree_type(...)`
   helper.
4. After cleanup, audit with:

```sh
rg 'ptree\("Int"|ptree\("Double"|ptree\("Tuple"|ptree\("List"' src/models src/bali-phy
rg 'parse_type\(.*ptree|unparse_type\(const ptree|type_from_ptree|ptree_from_type' src/models src/bali-phy
rg 'get_type_apps\(|get_type_head\(|make_type_app\(|make_type_apps\(' src/models src/bali-phy
```

Remaining `ptree` uses should be unrelated to model types.

## Validation

Run focused checks after each implementation batch:

```sh
meson test -C ../build/gcc-16-debug-O "bali-phy:model expression AST" --print-errorlogs
meson test -C ../build/gcc-16-debug-O "bali-phy:runtime AST serialization" --print-errorlogs
meson test -C ../build/gcc-16-debug-O "bali-phy:bali-phy 5d +A 50" --print-errorlogs
```

When parser or rule loading changes, also run relevant parse tests through Meson
and at least one command-line model smoke test that loads the binding database.
