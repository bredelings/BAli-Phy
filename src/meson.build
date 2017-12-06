boost= dependency('boost', modules : [ 'program_options', 'random', 'system','filesystem','chrono'])

libsums = static_library('libsums','computation/machine/sums.cc', cpp_args: '-fno-fast-math', install: false)

gitversion_h = vcs_tag(
  input : 'git-version.h.in',
  output: 'git-version.h',
  command: [ 'git', 'log', '-n1', '--date=format:"%b %d %Y %k:%M:%S"', '--pretty=format:[%D commit %h]' ],
  replace_string: '@git_message@',
  fallback: '',
)

baliphy_sources = ['parser/parse.cc','dp/dp_hmm.cc','parser/desugar.cc','sequence/sequence.cc',
   'tree/tree.cc','alignment/alignment.cc','substitution/substitution.cc',
   'mcmc/moves.cc', 'rng.cc','math/exponential.cc','math/eigenvalue.cc',
   'models/parameters.cc','prior.cc','mcmc/mcmc.cc', 'probability/choose.cc',
   'tree/sequencetree.cc','mcmc/sample-branch-lengths.cc', 'util.cc',
   'tree/randomtree.cc','sequence/alphabet.cc','bali-phy.cc',
   'models/TreeInterface.cc', 'dp/hmm.cc','dp/dp-engine.cc','dp/dp-array.cc',
   'dp/dp-matrix.cc','dp/3way.cc','dp/2way.cc','mcmc/sample-alignment.cc',
   'mcmc/sample-node.cc','imodel/imodel.cc','dp/5way.cc','mcmc/sample-topology-NNI.cc',
   'setup.cc','mcmc/sample-two-nodes.cc','sequence/sequence-format.cc',
   'util-random.cc','alignment/alignment-random.cc','models/setup.cc',
   'mcmc/sample-topology-SPR.cc', 'dp/alignment-sums.cc','alignment/alignment-util.cc',
   'probability/probability.cc','models/model.cc', 'alignment/alignment-constraint.cc',
   'monitor.cc', 'tree/tree-util.cc','myexception.cc','math/pow2.cc',
   'substitution/parsimony.cc', 'tools/partition.cc','mcmc/proposals.cc','n_indels.cc',
   'n_indels2.cc','alignment/alignment-util2.cc', 'tools/parsimony.cc',
   'tools/parsimony2.cc','version.cc','mcmc/slice-sampling.cc','timer_stack.cc',
   'mcmc/setup.cc','io.cc','mcmc/logger.cc','mcmc/AIS.cc','computation/operator.cc',
   'computation/expression/expression.cc','computation/expression/constructor.cc',
   'computation/expression/expression_ref.cc','computation/expression/AST_node.cc',
   'computation/expression/apply.cc','computation/expression/substitute.cc',
   'computation/expression/indexify.cc','computation/expression/let.cc',
   'computation/expression/case.cc','computation/expression/trim.cc',
   'computation/expression/tuple.cc','computation/expression/list.cc',
   'computation/expression/dummy.cc','computation/expression/lambda.cc',
   'computation/computation.cc','computation/machine/tokens.cc',
   'computation/preprocess.cc', 'computation/machine/graph_register.cc',
   'computation/machine/show_graph.cc','computation/module.cc',
   'computation/model_expression.cc', 'computation/machine/evaluate.cc',
   'computation/machine/gc.cc','computation/machine/reroot.cc',
   'computation/operations.cc','computation/loader.cc','computation/context.cc',
   'computation/closure.cc', 'computation/optimization/let-float.cc',
   'computation/program.cc','mcmc/sample-tri.cc','startup/A-T-model.cc',
   'startup/files.cc', 'startup/loggers.cc','startup/system.cc','startup/cmd_line.cc',
   'models/rules.cc','models/parse.cc', 'models/translate.cc','models/unification.cc',
   'computation/optimization/simplifier.cc', 'tools/read-trees.cc','alignment/load.cc',
   'alignment/index-matrix.cc','startup/paths.cc', 'computation/optimization/occurrence.cc',
   'computation/optimization/inliner.cc']

baliphy = executable('bali-phy',
		     baliphy_sources,
		     include_directories: eigen,
		     dependencies:boost,
		     link_with: libsums,
		     implib: true)

modelp_sources = ['tools/model_P.cc', 'tools/statistics.cc', 'rng.cc', 'util.cc']
modelp = executable('model_P', modelp_sources, dependencies: boost)

#compile_args += ['-Ipath/to/include']
#link_args += ['-Lpath/to/lib', '-lfoo', '-lbar']
#add_project_arguments(libahal_compile_args, language: 'c')
#add_project_link_arguments(link_args, language: 'c')
