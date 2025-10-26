
computation_sources = [
  'object.cc',
  'closure.cc',
  'program.cc',
  'preprocess.cc',
  'module.cc', 'symbols.cc', 'data_con_info.cc', 'instance_info.cc',
  'loader.cc',
  'context.cc',
  'operations.cc',
  'param.cc',
  'fresh_vars.cc',
  'message.cc',

  'parser/driver.cc', 'parser/lexer.cc', 'parser/parser.cc',

  'rename/rename.cc', 'rename/expression.cc', 'rename/pattern.cc',
  'rename/decls.cc', 'rename/stmt.cc', 'rename/match.cc',
  'rename/infix.cc', 'rename/types.cc',

  'desugar/desugar.cc','desugar/desugar-case.cc',

  'haskell/haskell.cc', 'haskell/pattern.cc', 'haskell/var.cc',
  'haskell/literal.cc', 'haskell/ids.cc', 'haskell/type.cc',
  'haskell/core.cc', 'haskell/coretype.cc', 'haskell/extensions.cc',

  'core/wrap.cc', 'core/func.cc',
  
  'expression/expression.cc','expression/constructor.cc',
  'expression/expression_ref.cc', 'expression/core.cc',
  'expression/apply.cc','expression/substitute.cc',
  'expression/indexify.cc','expression/let.cc',
  'expression/case.cc','expression/trim.cc',
  'expression/tuple.cc','expression/list.cc',
  'expression/var.cc','expression/lambda.cc',
  'expression/modifiable.cc', 'expression/interchangeable.cc',
  'expression/reg_var.cc', 'expression/convert.cc',
  'expression/bool.cc',
  'expression/maybe.cc',
  'expression/do_block.cc',

  'machine/args.cc','machine/tokens.cc', 'machine/graph_register.cc',
  'machine/show_graph.cc', 'machine/evaluate.cc', 'machine/sums.cc',
  'machine/gc.cc','machine/reroot.cc', 'machine/effects.cc', 'machine/mapping.cc',
  'machine/gcobject.cc',

  'typecheck/kind.cc', 'typecheck/kindcheck.cc', 'typecheck/typecheck.cc', 'typecheck/substitution.cc',
  'typecheck/env.cc', 'typecheck/alphabetize.cc', 'typecheck/types.cc', 'typecheck/instance.cc',
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
				dependencies: [boost, libxxhash, eigen, libdl, math_dep, librange, immer, fmt, cereal])
