boost= dependency('boost', modules : [ 'program_options', 'random', 'system','filesystem','chrono'])

libsums = static_library('libsums','computation/machine/sums.cc', cpp_args: '-fno-fast-math', install: false)

vcs_tag(
  input : 'git-version.h.in',
  output: 'git-version.h',
  command: [ 'git', 'log', '-n1', '--date=format:%b %d %Y %k:%M:%S', '--pretty=format:[%D commit %h]  (%cd)' ],
  replace_string: '@git_message@',
  fallback: '',
)

libdl = cpp.find_library('dl')
extra_link_args = []
extra_includes = []
if host_machine.system() == 'linux'
  extra_link_args += [ '-rdynamic' ]
elif host_machine.system() == 'windows'
  # We should have a system of 'cygwin' for cygwin.
  baliphy_sources += [ 'dlfcn-win32/dlfcn.c' ]
  extra_includes = include_directories('dlfcn-win32/')
  libdl = []
endif


# tools/findroot.cc -> tools/optimize.cc
libbaliphy_sources = ['io.cc','util.cc','tree/sequencetree.cc','tree/tree.cc','sequence/alphabet.cc','sequence/sequence.cc','tree/tree-util.cc','tools/read-trees.cc','sequence/sequence-format.cc','alignment/alignment-util.cc','rng.cc','alignment/load.cc','alignment/alignment.cc','tools/statistics.cc','tools/partition.cc','tools/tree-dist.cc','alignment/alignment-random.cc','setup.cc','tree/randomtree.cc','util-random.cc','tools/parsimony.cc','alignment/index-matrix.cc','tools/mctree.cc','tools/stats-table.cc','tools/findroot.cc','tools/optimize.cc','tools/distance-report.cc','n_indels.cc','tools/inverse.cc','tools/joint-A-T.cc','tools/distance-methods.cc','tools/consensus-tree.cc']

libbaliphy = static_library('bali-phy', libbaliphy_sources, 
			    include_directories: eigen,
			    dependencies: [boost],
			    link_args: extra_link_args)

baliphy_sources = ['parser/parse.cc','dp/dp_hmm.cc','parser/desugar.cc',
   'substitution/substitution.cc',
   'mcmc/moves.cc', 'math/exponential.cc','math/eigenvalue.cc',
   'models/parameters.cc','prior.cc','mcmc/mcmc.cc', 'probability/choose.cc',
   'mcmc/sample-branch-lengths.cc',
   'bali-phy.cc',
   'models/TreeInterface.cc', 'dp/hmm.cc','dp/dp-engine.cc','dp/dp-array.cc',
   'dp/dp-matrix.cc','dp/3way.cc','dp/2way.cc','mcmc/sample-alignment.cc',
   'mcmc/sample-node.cc','imodel/imodel.cc','dp/5way.cc','mcmc/sample-topology-NNI.cc',
   'mcmc/sample-two-nodes.cc',
   'models/setup.cc',
   'mcmc/sample-topology-SPR.cc', 'dp/alignment-sums.cc',
   'probability/probability.cc','models/model.cc', 'alignment/alignment-constraint.cc',
   'monitor.cc', 'myexception.cc','math/pow2.cc',
   'substitution/parsimony.cc', 'mcmc/proposals.cc',
   'n_indels2.cc','alignment/alignment-util2.cc',
   'tools/parsimony2.cc','version.cc','mcmc/slice-sampling.cc','timer_stack.cc',
   'mcmc/setup.cc','mcmc/logger.cc','mcmc/AIS.cc','computation/operator.cc',
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
   'computation/optimization/simplifier.cc',
   'startup/paths.cc', 'computation/optimization/occurrence.cc',
   'computation/optimization/inliner.cc']


baliphy = executable('bali-phy',
		     baliphy_sources,
		     include_directories: [eigen,extra_includes],
		     dependencies: [boost,libdl],
		     link_args: extra_link_args,
		     link_with: [libbaliphy,libsums],
		     install: true,
		     implib: true)

#compile_args += ['-Ipath/to/include']
#link_args += ['-Lpath/to/lib', '-lfoo', '-lbar']
#add_project_arguments(libahal_compile_args, language: 'c')
#add_project_link_arguments(link_args, language: 'c')

#https://gitlab.gnome.org/chergert/gir2rst/blob/master/meson.build
subdir('builtins')

modelp_sources = ['tools/model_P.cc']
executable('model_P', modelp_sources, dependencies: boost, link_with: libbaliphy, install:true)

statreport_sources = ['tools/statreport.cc']
executable('statreport', statreport_sources, dependencies: boost, link_with:libbaliphy, install:true)

stats_merge_sources = ['tools/stats-merge.cc']
executable('stats-merge', stats_merge_sources, dependencies: boost, link_with:libbaliphy, install:true)

stats_select_sources = ['tools/stats-select.cc']
executable('stats-select', stats_select_sources, dependencies: boost, link_with:libbaliphy, install:true)

stats_cat_sources = ['tools/stats-cat.cc']
executable('stats-cat', stats_cat_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_gild_sources = ['tools/alignment-gild.cc']
executable('alignment-gild', alignment_gild_sources, link_with: libbaliphy, install: true)

alignment_median_sources = ['tools/alignment-median.cc']
executable('alignment-median', alignment_median_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_consensus_sources = ['tools/alignment-consensus.cc']
executable('alignment-consensus', alignment_consensus_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_max_sources = ['tools/alignment-max.cc']
executable('alignment-max', alignment_max_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_compare_sources = ['tools/alignment-compare.cc']
executable('alignment-compare', alignment_compare_sources, dependencies:boost, link_with:libbaliphy, install: true)

alignment_identity_sources = ['tools/alignment-identity.cc']
executable('alignment-identity', alignment_identity_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_thin_sources = ['tools/alignment-thin.cc']
executable('alignment-thin', alignment_thin_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_chop_internal_sources = ['tools/alignment-chop-internal.cc']
executable('alignment-chop-internal', alignment_chop_internal_sources, dependencies:boost, link_with:libbaliphy, install: true)

alignment_indices_sources = ['tools/alignment-indices.cc']
executable('alignment-indices', alignment_indices_sources, dependencies:boost, link_with:libbaliphy, install: true)

alignments_diff_sources = ['tools/alignments-diff.cc']
executable('alignments-diff', alignments_diff_sources, dependencies:boost, link_with:libbaliphy, install: true)

alignment_draw_sources = ['tools/alignment-draw.cc', 'tools/colors.cc']
executable('alignment-draw', alignment_draw_sources, dependencies:boost, link_with: libbaliphy, install: true)

joint_indels_sources = ['tools/joint-indels.cc']
executable('joint-indels', joint_indels_sources, dependencies:boost, link_with: libbaliphy, install: true)

joint_parsimony_sources = ['tools/joint-parsimony.cc']
executable('joint-parsimony', joint_parsimony_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_info_sources = ['tools/alignment-info.cc']
executable('alignment-info', alignment_info_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_smc_sources = ['tools/alignment-smc.cc']
executable('alignment-smc', alignment_smc_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_cat_sources = ['tools/alignment-cat.cc']
executable('alignment-cat', alignment_cat_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_translate_sources = ['tools/alignment-translate.cc']
executable('alignment-translate', alignment_translate_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_find_sources = ['tools/alignment-find.cc']
executable('alignment-find', alignment_find_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_convert_sources = ['tools/alignment-convert.cc']
executable('alignment-convert', alignment_convert_sources, dependencies:boost, link_with: libbaliphy, install: true)

alignment_find_conserved_sources = ['tools/alignment-find-conserved.cc']
executable('alignment-find-conserved', alignment_find_conserved_sources, dependencies:boost, link_with: libbaliphy, install: true)

trees_consensus_sources = ['tools/trees-consensus.cc']
executable('trees-consensus', trees_consensus_sources, dependencies:boost, link_with: libbaliphy, install: true)

trees_bootstrap_sources = ['tools/trees-bootstrap.cc', 'tools/bootstrap.cc']
executable('trees-bootstrap', trees_bootstrap_sources, dependencies:boost, link_with: libbaliphy, install: true)

partitions_supported_sources = ['tools/partitions-supported.cc']
executable('partitions-supported', partitions_supported_sources, dependencies:boost, link_with: libbaliphy, install: true)

draw_graph_sources = ['tools/draw-graph.cc']
executable('draw-graph', draw_graph_sources, dependencies:boost, link_with: libbaliphy, install: true)

tree_mean_lengths_sources = ['tools/tree-mean-lengths.cc']
executable('tree-mean-lengths', tree_mean_lengths_sources, dependencies:boost, link_with: libbaliphy, install: true)

mctree_mean_lengths_sources = ['tools/mctree-mean-lengths.cc']
executable('mctree-mean-lengths', mctree_mean_lengths_sources, dependencies:boost, link_with: libbaliphy, install: true)

trees_pair_distances_sources = ['tools/trees-pair-distances.cc']
executable('trees-pair-distances', trees_pair_distances_sources, dependencies:boost, link_with: libbaliphy, install: true)

tree_partitions_sources = ['tools/tree-partitions.cc']
executable('tree-partitions', tree_partitions_sources, dependencies:boost, link_with: libbaliphy, install: true)

trees_to_SRQ_sources = ['tools/trees-to-SRQ.cc']
executable('trees-to-SRQ', trees_to_SRQ_sources, dependencies:boost, link_with: libbaliphy, install: true)

tree_reroot_sources = ['tools/tree-reroot.cc']
executable('tree-reroot', tree_reroot_sources, dependencies:boost, link_with: libbaliphy, install: true)

pickout_sources = ['tools/pickout.cc', 'util.cc', 'io.cc']
executable('pickout', pickout_sources, dependencies:boost, link_with: libbaliphy, install: true)

subsample_sources = ['tools/subsample.cc']
executable('subsample', subsample_sources, dependencies:boost, link_with: libbaliphy, install: true)

cut_range_sources = ['tools/cut-range.cc']
executable('cut-range', cut_range_sources, dependencies:boost, link_with: libbaliphy, install: true)

trees_distances_sources = ['tools/trees-distances.cc']
executable('trees-distances', trees_distances_sources, dependencies:boost, link_with: libbaliphy, install: true)

if cairo.found()
  draw_tree_sources = ['tools/draw-tree.cc']
  executable('draw-tree', draw_tree_sources, dependencies: [boost, cairo], link_with: libbaliphy, install:true)
endif

path_graph_sources = ['tools/path-graph.cc']
executable('path-graph', path_graph_sources, dependencies:boost, link_with: libbaliphy, install: true)

