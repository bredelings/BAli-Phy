
computation_sources = [
  'object.cc',
  'byte-string.cc',
  'closure.cc',
  'program.cc',
  'preprocess.cc',
  'module.cc', 'symbols.cc', 'data_con_info.cc', 'instance_info.cc', 'record_utils.cc',
  'loader.cc',
  'context.cc',
  'operations.cc',
  'param.cc',
  'fresh_vars.cc',
  'message.cc',

  'parser/driver.cc', 'parser/lexer.cc', 'parser/parser.cc',

  'rename/rename.cc', 'rename/expression.cc', 'rename/pattern.cc',
  'rename/records.cc',
  'rename/disambiguate.cc',
  'rename/decls.cc', 'rename/stmt.cc', 'rename/match.cc',
  'rename/infix.cc', 'rename/types.cc',

  'desugar/desugar.cc','desugar/desugar-case.cc',

  'haskell/haskell.cc', 'haskell/pattern.cc', 'haskell/var.cc',
  'haskell/literal.cc', 'haskell/ids.cc', 'haskell/type.cc',
  'core/type.cc', 'haskell/extensions.cc', 'haskell/generated.cc',

  'core/wrap.cc', 'core/func.cc',
  
  'runtime/indexify.cc',
  'runtime/modifiable.cc', 'runtime/interchangeable.cc',
  'core/convert.cc',

  'machine/args.cc','machine/tokens.cc', 'machine/graph_register.cc',
  'machine/translate.cc',
  'machine/show_graph.cc', 'machine/evaluate.cc', 'machine/sums.cc',
  'machine/gc.cc','machine/reroot.cc', 'machine/effects.cc', 'machine/mapping.cc',
  'machine/gcobject.cc',

  'runtime/ast.cc',
  'runtime/trim.cc',

  'typecheck/kind.cc', 'typecheck/kindcheck.cc', 'typecheck/typecheck.cc', 'typecheck/substitution.cc',
  'typecheck/env.cc', 'typecheck/alphabetize.cc', 'typecheck/types.cc', 'typecheck/instance.cc', 'typecheck/derived.cc',
  'typecheck/class.cc', 'typecheck/expression.cc', 'typecheck/pattern.cc', 'typecheck/match.cc',
  'typecheck/quals.cc', 'typecheck/binds.cc', 'typecheck/solver.cc', 'typecheck/default.cc',
  'typecheck/type_groups.cc', 'typecheck/constraints.cc', 'typecheck/expected.cc',
  'typecheck/unify.cc', 'typecheck/solver/canonicalize.cc', 'typecheck/solver/rewrite.cc',
  'typecheck/context.cc', 'typecheck/errors.cc', 'typecheck/tidy.cc', 'typecheck/data.cc',

  'optimization/free-vars.cc', 'optimization/set-levels.cc', 'optimization/float-out.cc',
  'optimization/occurrence.cc', 'optimization/inliner.cc', 'optimization/simplifier.cc'
]



libcomputation = static_library('computation',
				computation_sources,
				include_directories: [root_inc, libutil_inc],
				dependencies: [boost, libxxhash, libzstd, eigen, libdl, math_dep, librange, immer, fmt, cereal, reflex_dep])
