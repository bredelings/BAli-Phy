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
- `type_t` is the command-line model alias for `CM::Type`; the old `term_t`
  alias has been removed.
- `Rule::result_type`, `RuleArg::type`, and `Rule::constraints` are native
  types after JSON binding loading.
- `CM::Ann::type` is native.
- `TypecheckingState`, unification, model typechecking, code generation,
  `model_t`, and the A/T model construction helpers use native types.
- The command-line type grammar in `parser.y` returns `CM::Type` directly, and
  `parse_type(...)` returns `CM::Type`.
- There is no production `ptree -> CM::Type` converter.
- `parse.cc` and `parse.H` no longer expose the obsolete `ptree` model display
  helpers.
- Rule loading reads `boost::json` into a private `RawRule` boundary and then
  converts directly to native `Rule` values.
- Rule citations are stored as native `RuleCitation` data, not as `ptree`.
- Type application call sites use `CM::type_app(...)` and
  `CM::type_apps(...)` directly; the old global `make_type_app(...)` helper
  names have been removed.

`ptree` remains in nearby code for the unrelated help-topic tree in
`src/bali-phy/help.cc` and the parameter-name trie in `path.cc`.

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

1. Optionally replace the `path.cc` parameter-name trie with a small local node
   struct.  This use is unrelated to model expressions and types.
2. Optionally replace the help-topic tree in `src/bali-phy/help.cc` with a small
   local node struct.  This use is unrelated to rule loading and model
   expressions.
3. After cleanup, audit with:

```sh
rg 'ptree\("Int"|ptree\("Double"|ptree\("Tuple"|ptree\("List"' src/models src/bali-phy
rg 'parse_type\(.*ptree|unparse_type\(const ptree|type_from_ptree|ptree_from_type' src/models src/bali-phy
rg 'get_type_apps\(|get_type_head\(|make_type_app\(|make_type_apps\(' src/models src/bali-phy
rg '\bptree\b|ptree_' src/models src/bali-phy
```

Remaining `ptree` uses should be unrelated to command-line model expressions,
types, and rule loading.

## Rule Loader `ptree` Removal Plan

Done.  The rule loader now parses binding JSON with `boost::json`, stores it
temporarily as a private `RawRule`, and converts directly to native `Rule`
values.  `json_to_ptree(...)`, `ptree` rule field helpers, and `ptree` citation
storage have been removed.

The binding JSON files now use array spellings for `args` and `constraints`, so
the loader rejects scalar spellings for those fields.

## Validation

Run focused checks after each implementation batch:

```sh
meson test -C ../build/gcc-16-debug-O "bali-phy:model expression AST" --print-errorlogs
meson test -C ../build/gcc-16-debug-O "bali-phy:runtime AST serialization" --print-errorlogs
meson test -C ../build/gcc-16-debug-O "bali-phy:bali-phy 5d +A 50" --print-errorlogs
```

When parser or rule loading changes, also run relevant parse tests through Meson
and at least one command-line model smoke test that loads the binding database.
