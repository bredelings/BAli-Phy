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
- `parse.cc` and `parse.H` no longer expose the obsolete `ptree` model display
  helpers.

`ptree` remains in nearby code for raw JSON rule loading, citations/docs, and
the unrelated parameter-name trie in `path.cc`.

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

1. Remove `ptree` from rule loading and documentation metadata.
2. Consider whether the global `make_type_app(...)` / `make_type_apps(...)`
   compatibility names should be renamed or moved into `CM` once call sites are
   easy to update.  They are native helpers now, not ptree bridges.
3. Optionally replace the `path.cc` parameter-name trie with a small local node
   struct.  This use is unrelated to model expressions and types.
4. After cleanup, audit with:

```sh
rg 'ptree\("Int"|ptree\("Double"|ptree\("Tuple"|ptree\("List"' src/models src/bali-phy
rg 'parse_type\(.*ptree|unparse_type\(const ptree|type_from_ptree|ptree_from_type' src/models src/bali-phy
rg 'get_type_apps\(|get_type_head\(|make_type_app\(|make_type_apps\(' src/models src/bali-phy
rg '\bptree\b|ptree_' src/models src/bali-phy
```

Remaining `ptree` uses should be unrelated to command-line model expressions,
types, and rule loading.

## Rule Loader `ptree` Removal Plan

The rule loader currently parses binding JSON with `boost::json`, converts it to
`ptree`, then immediately reads fields from that `ptree` to construct native
`Rule` values.  Replace that conversion boundary directly; do not add a second
rule-loading path.

1. Keep rule-file parsing in `Rules::add_rule_json(...)`, but store raw rules as
   `boost::json::object` or a small `RawRule` struct instead of `ptree`.
   Preserve the existing behavior that injects the directory-derived
   `"category"` array before storing the raw rule.
2. Replace the `ptree` field helpers with JSON helpers used by the existing
   loader:
   - `required_string(object, key, rule_name)`
   - `optional_string(object, key)`
   - `string_array(object, key, rule_name)`
   - `optional_array/object` validators where needed
   These helpers should throw the same style of rule-name-qualified errors as
   the current code.
3. Convert `parse_constraints(...)`, `get_string_array(...)`, `get_imports(...)`,
   `make_rule_stub(...)`, and `convert_rule(...)` to take the native JSON/raw
   rule representation.  Continue to parse all type strings immediately into
   `CM::Type` and all expression strings immediately into `CM::Expr`.
4. Replace `RuleDocs::citation = optional<ptree>` with native documentation
   data, not a new long-lived raw JSON wrapper.  A minimal native shape should
   cover the schemas used by `help.cc`: string citations, authors with names,
   title, year, identifiers with `type`/`id`, and links with `url`/`anchor`.
5. Update `src/bali-phy/help.cc` to read the native citation data.  Once this
   compiles, delete `json_to_ptree(...)` and remove `util/ptree.H` from
   `rules.H` and `rules.cc`.
6. Validate with the model-expression test, the runtime AST serialization test,
   the `5d +A` test, and a Meson parse-test run.  Rule loading should also be
   smoke-tested through a command that constructs `Rules` from the bindings
   database, because most of the behavior being changed happens at startup.

## Validation

Run focused checks after each implementation batch:

```sh
meson test -C ../build/gcc-16-debug-O "bali-phy:model expression AST" --print-errorlogs
meson test -C ../build/gcc-16-debug-O "bali-phy:runtime AST serialization" --print-errorlogs
meson test -C ../build/gcc-16-debug-O "bali-phy:bali-phy 5d +A 50" --print-errorlogs
```

When parser or rule loading changes, also run relevant parse tests through Meson
and at least one command-line model smoke test that loads the binding database.
