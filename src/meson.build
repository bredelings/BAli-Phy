boost= dependency('boost', modules : [ 'program_options', 'random', 'system','filesystem','chrono'])

libsums = static_library('libsums','computation/machine/sums.cc', cpp_args: '-fno-fast-math', install: false)

gitversion_h = vcs_tag(
  input : 'git-version.h.in',
  output: 'git-version.h',
  command: [ 'git', 'log', '-n1', '--date=format:%b %d %Y %k:%M:%S', '--pretty=format:[%D commit %h]  (%cd)' ],
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
		     dependencies:[boost,libdl],
		     link_with: libsums,
		     install: true,
		     implib: true)

#compile_args += ['-Ipath/to/include']
#link_args += ['-Lpath/to/lib', '-lfoo', '-lbar']
#add_project_arguments(libahal_compile_args, language: 'c')
#add_project_link_arguments(link_args, language: 'c')

#https://gitlab.gnome.org/chergert/gir2rst/blob/master/meson.build
subdir('builtins')

modelp_sources = ['tools/model_P.cc', 'tools/statistics.cc', 'rng.cc', 'util.cc']
executable('model_P', modelp_sources, dependencies: boost, install:true)

statreport_sources = ['tools/statreport.cc', 'tools/statistics.cc', 'util.cc',
		      'tools/stats-table.cc', 'io.cc']
executable('statreport', statreport_sources, dependencies: boost, install:true)

stats_merge_sources = ['tools/stats-merge.cc','util.cc','io.cc']
executable('stats-merge', stats_merge_sources, dependencies: boost, install:true)

stats_select_sources = ['tools/stats-select.cc','util.cc','tools/stats-table.cc','io.cc']
executable('stats-select', stats_select_sources, dependencies: boost, install:true)

stats_cat_sources = ['tools/stats-cat.cc', 'util.cc', 'tools/stats-table.cc' ,'io.cc']
executable('stats-cat', stats_cat_sources, dependencies:boost, install: true)

alignment_gild_sources = ['tools/alignment-gild.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc',
			  'sequence/sequence.cc', 'util.cc', 'rng.cc', 'tree/tree.cc', 'tree/sequencetree.cc',
			  'tools/optimize.cc', 'tools/findroot.cc', 'setup.cc', 'sequence/sequence-format.cc',
			  'tools/distance-methods.cc', 'alignment/alignment-random.cc', 'alignment/alignment-util.cc',
			  'tree/randomtree.cc', 'tree/tree-util.cc', 'tools/inverse.cc', 'io.cc', 'tools/read-trees.cc',
			  'alignment/load.cc']
executable('alignment-gild', alignment_gild_sources, include_directories: eigen, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_median_sources = ['tools/alignment-median.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc',
	'tree/tree.cc', 'tree/sequencetree.cc', 'sequence/sequence-format.cc', 'alignment/alignment-util.cc', 'io.cc', 'tools/distance-report.cc', 'tools/statistics.cc',
	'alignment/load.cc']
executable('alignment-median', alignment_median_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_consensus_sources = ['tools/alignment-consensus.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'rng.cc',
	'tree/tree.cc', 'tree/sequencetree.cc', 'util-random.cc', 'tools/statistics.cc',
	'sequence/sequence-format.cc', 'alignment/alignment-util.cc', 'alignment/index-matrix.cc', 'io.cc', 'alignment/load.cc']
executable('alignment-consensus', alignment_consensus_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_max_sources = ['tools/alignment-max.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'rng.cc',
	'tree/tree.cc', 'tree/sequencetree.cc', 'util-random.cc', 'tools/statistics.cc',
	'sequence/sequence-format.cc', 'alignment/alignment-util.cc', 'alignment/index-matrix.cc', 'io.cc', 'alignment/load.cc']
executable('alignment-max', alignment_consensus_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_compare_sources = ['tools/alignment-compare.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'rng.cc',
	'tree/tree.cc', 'tree/sequencetree.cc', 'util-random.cc', 'sequence/sequence-format.cc', 'alignment/alignment-util.cc', 'io.cc',
	'alignment/load.cc']
executable('alignment-compare', alignment_compare_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_identity_sources = ['tools/alignment-identity.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'rng.cc',
	'tree/tree.cc', 'tree/sequencetree.cc', 'util-random.cc', 'tools/statistics.cc', 'alignment/load.cc',
	'sequence/sequence-format.cc', 'alignment/alignment-util.cc', 'alignment/index-matrix.cc', 'io.cc']
executable('alignment-identity', alignment_identity_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_thin_sources = ['tools/alignment-thin.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'rng.cc',
	'tree/tree.cc', 'tree/sequencetree.cc', 'setup.cc', 'sequence/sequence-format.cc', 'tree/randomtree.cc',
	'alignment/alignment-util.cc', 'alignment/alignment-random.cc', 'tree/tree-util.cc', 'alignment/load.cc',
	'tools/distance-methods.cc', 'tools/inverse.cc', 'alignment/index-matrix.cc', 'io.cc', 'tools/read-trees.cc']
executable('alignment-thin', alignment_thin_sources, include_directories: eigen, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_chop_internal_sources = ['tools/alignment-chop-internal.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'tree/tree.cc',
	'sequence/sequence-format.cc', 'alignment/alignment-util.cc', 'io.cc', 'tree/sequencetree.cc', 'alignment/load.cc', 'tree/tree-util.cc', 'tools/read-trees.cc']
executable('alignment-chop-internal', alignment_chop_internal_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_indices_sources = ['tools/alignment-indices.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'tree/tree.cc', 'sequence/sequence-format.cc', 'alignment/alignment-util.cc', 'io.cc', 'tree/sequencetree.cc', 'alignment/load.cc']
executable('alignment-indices', alignment_indices_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignments_diff_sources = ['tools/alignments-diff.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'tree/tree.cc', 'sequence/sequence-format.cc', 'alignment/alignment-util.cc', 'io.cc', 'tree/sequencetree.cc', 'alignment/load.cc']
executable('alignments-diff', alignments_diff_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_draw_sources = ['tools/alignment-draw.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'sequence/sequence-format.cc', 'util.cc', 'alignment/alignment-util.cc', 'tools/colors.cc', 'tree/tree.cc', 'io.cc', 'tree/sequencetree.cc', 'alignment/load.cc']
executable('alignment-draw', alignment_draw_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

joint_indels_sources = ['tools/joint-indels.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'rng.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tree/tree-util.cc', 'setup.cc', 'sequence/sequence-format.cc', 'alignment/alignment-random.cc', 'alignment/alignment-util.cc', 'tree/randomtree.cc', 'tools/statistics.cc', 'tools/joint-A-T.cc', 'tools/partition.cc', 'io.cc', 'tools/read-trees.cc', 'alignment/load.cc']
executable('joint-indels', joint_indels_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

joint_parsimony_sources = ['tools/joint-parsimony.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'rng.cc', 'tree/tree.cc',
	'tree/sequencetree.cc', 'tree/tree-util.cc', 'setup.cc', 'sequence/sequence-format.cc',
	'alignment/alignment-random.cc', 'alignment/alignment-util.cc', 'tree/randomtree.cc',
	'tools/parsimony.cc', 'tools/joint-A-T.cc', 'n_indels.cc', 'io.cc', 'tools/read-trees.cc', 'alignment/load.cc']
executable('joint-parsimony', joint_parsimony_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_info_sources = ['tools/alignment-info.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'rng.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'setup.cc', 'tools/parsimony.cc', 'sequence/sequence-format.cc', 'tree/randomtree.cc', 'alignment/alignment-util.cc', 'alignment/alignment-random.cc', 'tree/tree-util.cc', 'tools/statistics.cc', 'io.cc', 'tools/read-trees.cc', 'alignment/load.cc']
executable('alignment-info', alignment_info_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_smc_sources = ['tools/alignment-smc.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'rng.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'setup.cc', 'tools/parsimony.cc', 'sequence/sequence-format.cc', 'tree/randomtree.cc', 'alignment/alignment-util.cc', 'alignment/alignment-random.cc', 'tree/tree-util.cc', 'tools/statistics.cc', 'io.cc', 'tools/read-trees.cc', 'alignment/load.cc']
executable('alignment-smc', alignment_smc_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_cat_sources = ['tools/alignment-cat.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'sequence/sequence-format.cc', 'io.cc',
	'tree/tree.cc', 'tree/sequencetree.cc', 'tools/optimize.cc', 'tools/findroot.cc']
executable('alignment-cat', alignment_cat_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_translate_sources = ['tools/alignment-translate.cc', 'alignment/alignment.cc', 'alignment/alignment-util.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'sequence/sequence-format.cc', 'util.cc', 'tree/tree.cc', 'setup.cc', 'tree/sequencetree.cc', 'tree/randomtree.cc', 'rng.cc', 'tree/tree-util.cc', 'alignment/alignment-random.cc', 'io.cc', 'tools/tree-dist.cc', 'tools/partition.cc', 'tools/read-trees.cc', 'alignment/load.cc']
executable('alignment-translate', alignment_translate_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_find_sources = ['tools/alignment-find.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'alignment/alignment-util.cc', 'rng.cc', 'util.cc', 'sequence/sequence-format.cc', 'tree/tree.cc', 'io.cc', 'tree/sequencetree.cc', 'alignment/load.cc']
executable('alignment-find', alignment_find_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_convert_sources = ['tools/alignment-convert.cc', 'alignment/alignment.cc', 'alignment/alignment-util.cc', 'sequence/sequence.cc', 'sequence/alphabet.cc', 'util.cc', 'sequence/sequence-format.cc', 'tree/tree.cc', 'io.cc', 'tree/sequencetree.cc', 'alignment/load.cc']
executable('alignment-convert', alignment_convert_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

alignment_find_conserved_sources = ['tools/alignment-find-conserved.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc', 'rng.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'setup.cc', 'tools/parsimony.cc', 'sequence/sequence-format.cc', 'tree/randomtree.cc', 'alignment/alignment-util.cc', 'alignment/alignment-random.cc', 'tree/tree-util.cc', 'tools/statistics.cc', 'tools/partition.cc', 'io.cc', 'tools/read-trees.cc', 'alignment/load.cc']
executable('alignment-find-conserved', alignment_find_conserved_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

trees_consensus_sources = ['tools/trees-consensus.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tools/tree-dist.cc', 'util.cc', 'tools/statistics.cc', 'tree/tree-util.cc', 'tools/mctree.cc', 'rng.cc', 'tools/partition.cc', 'tools/consensus-tree.cc', 'io.cc', 'tools/read-trees.cc']
executable('trees-consensus', trees_consensus_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

trees_bootstrap_sources = ['tools/trees-bootstrap.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tools/tree-dist.cc', 'util.cc', 'rng.cc', 'tools/statistics.cc', 'tools/bootstrap.cc', 'tree/tree-util.cc', 'tools/partition.cc', 'tools/consensus-tree.cc', 'io.cc', 'tools/read-trees.cc']
executable('trees-bootstrap', trees_bootstrap_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

partitions_supported_sources = ['tools/partitions-supported.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tools/tree-dist.cc', 'util.cc', 'tools/statistics.cc', 'tree/tree-util.cc', 'tools/partition.cc', 'io.cc', 'tools/read-trees.cc']
executable('partitions-supported', partitions_supported_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

draw_graph_sources = ['tools/draw-graph.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tools/tree-dist.cc', 'util.cc', 'tree/tree-util.cc', 'tools/mctree.cc', 'rng.cc', 'tools/partition.cc', 'io.cc', 'tools/read-trees.cc']
executable('draw-graph', draw_graph_sources, dependencies:boost, install: true)

#---------------------------------------------------------------
tree_mean_lengths_sources = ['tools/tree-mean-lengths.cc', 'util.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tools/tree-dist.cc', 'tools/statistics.cc', 'tree/tree-util.cc', 'tools/partition.cc', 'io.cc', 'tools/read-trees.cc']
executable('tree-mean-lengths', tree_mean_lengths_sources, dependencies:boost, install: true)

#---------------------------------------------------------------
mctree_mean_lengths_sources = ['tools/mctree-mean-lengths.cc', 'util.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tools/tree-dist.cc', 'tools/statistics.cc', 'tree/tree-util.cc', 'tools/mctree.cc', 'rng.cc', 'tools/partition.cc', 'io.cc', 'tools/read-trees.cc']
executable('mctree-mean-lengths', mctree_mean_lengths_sources, dependencies:boost, install: true)

#---------------------------------------------------------------
trees_pair_distances_sources = ['tools/trees-pair-distances.cc', 'util.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tools/tree-dist.cc', 'tools/statistics.cc', 'tree/tree-util.cc', 'tools/partition.cc', 'io.cc', 'tools/read-trees.cc']
executable('trees-pair-distances', trees_pair_distances_sources, dependencies:boost, install: true)

#---------------------------------------------------------------
tree_partitions_sources = ['tools/tree-partitions.cc', 'util.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tools/tree-dist.cc', 'tree/tree-util.cc', 'tools/partition.cc', 'io.cc', 'tools/read-trees.cc']
executable('tree-partitions', tree_partitions_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

trees_to_SRQ_sources = ['tools/trees-to-SRQ.cc', 'util.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tools/tree-dist.cc', 'tools/read-trees.cc', 'tools/statistics.cc', 'tree/tree-util.cc', 'tools/partition.cc', 'io.cc']
executable('trees-to-SRQ', trees_to_SRQ_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

tree_reroot_sources = ['tools/tree-reroot.cc', 'tree/tree.cc', 'tree/sequencetree.cc', 'tree/tree-util.cc', 'util.cc', 'tools/tree-dist.cc', 'tools/partition.cc', 'io.cc', 'tools/read-trees.cc']
executable('tree-reroot', tree_reroot_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

pickout_sources = ['tools/pickout.cc', 'util.cc', 'io.cc']
executable('pickout', pickout_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

subsample_sources = ['tools/subsample.cc', 'io.cc']
executable('subsample', subsample_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

cut_range_sources = ['tools/cut-range.cc', 'util.cc', 'io.cc']
executable('cut-range', cut_range_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

#analyze_distances_sources = ['tools/analyze_distances.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc',
#	sequence/sequence.cc', 'tools/distance-methods.cc',
#	util.cc', 'tree/sequencetree.cc', 'substitution/substitution.cc', 'eigenvalue.cc', 'tree/tree.cc',
#	exponential.cc', 'models/setup.cc', 'models/smodel.cc', 'imodel.cc', 'rng.cc', 'prior.cc',
#	choose.cc', 'tools/optimize.cc', 'setup.cc', 'models/rates.cc', 'alignment/alignment-util.cc',
#	sequence/sequence-format.cc', 'tree/randomtree.cc', 'model.cc', 'computation/graph_register.cc', ' probability.cc',
#	substitution/substitution-cache.cc', 'substitution/substitution-index.cc', 'substitution/substitution-star.cc', 'tree/tree-util.cc',
#	alignment-random.cc', 'parameters.cc', 'myexception.cc', 'monitor.cc',
#	tools/tree-dist.cc', 'tools/inverse.cc', 'distribution.cc', 'tools/partition.cc',
#	timer_stack.cc', 'io.cc', 'computation/operator.cc', 'computation/expression.cc', 'models/objects.cc',
#	computation/computation.cc', 'computation/model_expression.cc', 'models/operations.cc',
#	distribution-operations.cc', '2way.cc', 'computation/operations.cc', 'computation/module.cc',
#	computation/loader.cc', 'computation/context.cc

#---------------------------------------------------------------

trees_distances_sources = ['tools/trees-distances.cc', 'tree/tree.cc',
	'tree/sequencetree.cc', 'tools/tree-dist.cc', 'tools/partition.cc', 'util.cc',
	'tree/tree-util.cc', 'tools/statistics.cc', 'io.cc', 'tools/distance-report.cc', 'tools/read-trees.cc', 'rng.cc']
executable('trees-distances', trees_distances_sources, dependencies:boost, install: true)

#---------------------------------------------------------------

draw_tree_sources = ['tools/draw-tree.cc', 'tree/tree.cc', 'tree/sequencetree.cc',
	'tools/tree-dist.cc', 'util.cc', 'tree/tree-util.cc', 'tools/mctree.cc', 'rng.cc',
	'util-random.cc', 'tools/partition.cc', 'io.cc', 'tools/read-trees.cc']

executable('draw-tree', modelp_sources, dependencies: [boost,cairo], install:true)

#---------------------------------------------------------------

path_graph_sources = ['tools/path-graph.cc', 'alignment/alignment.cc', 'sequence/alphabet.cc', 'sequence/sequence.cc', 'util.cc',
	'sequence/sequence-format.cc', 'alignment/alignment-util.cc', 'tree/tree.cc', 'io.cc', 'tree/sequencetree.cc', 'alignment/load.cc']
executable('path-graph', path_graph_sources, dependencies:boost, install: true)

#---------------------------------------------------------------
