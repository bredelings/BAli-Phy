
computation_sources = [
  'object.cc',
  'closure.cc',
  'program.cc',
  'preprocess.cc',
  'module.cc',
  'loader.cc',
  'context.cc',
  'operations.cc',
  'param.cc',

  'parser/desugar.cc','parser/desugar-case.cc', 'parser/rename.cc',
  'parser/driver.cc', 'parser/lexer.cc', 'parser/parser.cc',
  'parser/haskell.cc',

  'expression/operator.cc', 'expression/expression.cc','expression/constructor.cc',
  'expression/expression_ref.cc','expression/AST_node.cc',
  'expression/apply.cc','expression/substitute.cc',
  'expression/indexify.cc','expression/let.cc',
  'expression/case.cc','expression/trim.cc',
  'expression/tuple.cc','expression/list.cc',
  'expression/var.cc','expression/lambda.cc',
  'expression/modifiable.cc',
  'expression/reg_var.cc',
  'expression/bool.cc', 'expression/resolved_symbol.cc',
  'expression/maybe.cc',
  'expression/do_block.cc',

  'machine/args.cc','machine/tokens.cc', 'machine/graph_register.cc',
  'machine/show_graph.cc',
  'machine/evaluate.cc', 'machine/sums.cc',
  'machine/gc.cc','machine/reroot.cc', 'machine/effects.cc',

  'expression/kind.cc',

  'optimization/free-vars.cc', 'optimization/set-levels.cc', 'optimization/float-out.cc',

  'optimization/occurrence.cc', 'optimization/inliner.cc', 'optimization/simplifier.cc'
]



libcomputation = static_library('computation',
				computation_sources,
				include_directories: [root_inc, libutil_inc],
				dependencies: [boost, eigen, json, libdl, math_dep, librange, immer])
