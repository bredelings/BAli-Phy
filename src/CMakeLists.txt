set(BOOST_LIBS boost_program_options boost_filesystem boost_chrono boost_system)

set(GIT_VERSION_CMD ${PROJECT_SOURCE_DIR}/scripts/git_version.sh)

add_custom_command (
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/git_version.h
  COMMAND ${GIT_VERSION_CMD} -k -s ${PROJECT_SOURCE_DIR} -o ${PROJECT_BINARY_DIR}/git_version.h
#  DEPENDS MakeTable
  )

include_directories( ${CMAKE_CURRENT_BINARY_DIR} )

add_executable(bali-phy sequence.C tree.C alignment.C substitution.C
  moves.C rng.C exponential.C eigenvalue.C parameters.C likelihood.C
  mcmc.C choose.C sequencetree.C sample-branch-lengths.C util.C
  randomtree.C alphabet.C smodel/smodel.C bali-phy.C hmm.C dp-engine.C
  dp-array.C dp-matrix.C 3way.C 2way.C sample-alignment.C sample-tri.C
  sample-node.C imodel.C 5way.C sample-topology-NNI.C setup.C rates.C
  matcache.C sample-two-nodes.C sequence-format.C util-random.C
  alignment-random.C setup-smodel.C sample-topology-SPR.C
  alignment-sums.C alignment-util.C probability.C model.C
  alignment-constraint.C substitution-cache.C substitution-star.C
  monitor.C substitution-index.C tree-util.C myexception.C pow2.C
  tools/partition.C proposals.C n_indels.C distribution.C
  tools/parsimony.C version.C slice-sampling.C timer_stack.C
  setup-mcmc.C io.C logger.C AIS.C operator.C expression.C formula.C
  setup-imodel.C smodel/objects.C context.C computation.C
  formula_expression.C smodel/operations.C distribution-operations.C
  operations.C git_version.h) 
target_link_libraries(bali-phy ${BOOST_LIBS})

add_executable(model_P tools/model_P.C tools/statistics.C rng.C util.C)
target_link_libraries(model_P ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(statreport tools/statreport.C tools/statistics.C util.C tools/stats-table.C io.C)
target_link_libraries(statreport ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(stats_merge tools/stats-merge.C util.C io.C)
target_link_libraries(stats_merge ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(stats_select tools/stats-select.C util.C tools/stats-table.C io.C)
target_link_libraries(stats_select ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(stats_cat tools/stats-cat.C util.C tools/stats-table.C io.C)
target_link_libraries(stats_cat ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(analyze_rates tools/analyze-rates.C util.C tools/stats-table.C
	tools/statistics.C io.C)
target_link_libraries(analyze_rates ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_gild tools/alignment-gild.C alignment.C alphabet.C
	sequence.C util.C rng.C tree.C sequencetree.C tools/optimize.C
	tools/findroot.C setup.C sequence-format.C
	tools/distance-methods.C alignment-random.C alignment-util.C
	randomtree.C tree-util.C tools/inverse.C io.C)

target_link_libraries(alignment_gild  ${ATLAS_LIBS} ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_median tools/alignment-median.C alignment.C
	alphabet.C sequence.C util.C tree.C sequencetree.C
	sequence-format.C alignment-util.C io.C
	tools/distance-report.C tools/statistics.C)
target_link_libraries(alignment_median ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_consensus tools/alignment-consensus.C
	alignment.C alphabet.C sequence.C util.C rng.C tree.C
	sequencetree.C util-random.C tools/statistics.C
	sequence-format.C alignment-util.C tools/index-matrix.C io.C)
target_link_libraries(alignment_consensus ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_max tools/alignment-max.C alignment.C
	alphabet.C sequence.C util.C rng.C tree.C sequencetree.C
	util-random.C tools/statistics.C sequence-format.C
	alignment-util.C tools/index-matrix.C io.C)
target_link_libraries(alignment_max ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_compare tools/alignment-compare.C alignment.C
	alphabet.C sequence.C util.C rng.C tree.C sequencetree.C
	util-random.C sequence-format.C alignment-util.C io.C)
target_link_libraries(alignment_compare ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_identity tools/alignment-identity.C
	alignment.C alphabet.C sequence.C util.C rng.C tree.C
	sequencetree.C util-random.C tools/statistics.C
	sequence-format.C alignment-util.C tools/index-matrix.C io.C) 
target_link_libraries(alignment_identity ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_reorder tools/alignment-reorder.C alignment.C
	alphabet.C sequence.C util.C rng.C tree.C sequencetree.C
	tools/optimize.C tools/findroot.C setup.C sequence-format.C
	randomtree.C alignment-util.C alignment-random.C tree-util.C io.C)
target_link_libraries(alignment_reorder ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_thin tools/alignment-thin.C alignment.C
  alphabet.C sequence.C util.C rng.C tree.C sequencetree.C setup.C
  sequence-format.C randomtree.C alignment-util.C alignment-random.C
  tree-util.C tools/distance-methods.C tools/inverse.C
  tools/index-matrix.C io.C) 
target_link_libraries(alignment_thin ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_chop_internal tools/alignment-chop-internal.C
  alignment.C alphabet.C sequence.C util.C tree.C sequence-format.C
  alignment-util.C io.C sequencetree.C) 
target_link_libraries(alignment_chop_internal ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_indices tools/alignment-indices.C alignment.C
  alphabet.C sequence.C util.C tree.C sequence-format.C
  alignment-util.C io.C sequencetree.C)
target_link_libraries(alignment_indices ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignments_diff tools/alignments-diff.C alignment.C
  alphabet.C sequence.C util.C tree.C sequence-format.C
  alignment-util.C io.C sequencetree.C)
target_link_libraries(alignments_diff ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_draw tools/alignment-draw.C alignment.C
  alphabet.C sequence.C sequence-format.C util.C alignment-util.C
  tools/colors.C tree.C io.C sequencetree.C) 
target_link_libraries(alignment_draw ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(joint_indels tools/joint-indels.C alignment.C
  alphabet.C sequence.C util.C rng.C tree.C sequencetree.C tree-util.C
  setup.C sequence-format.C alignment-random.C alignment-util.C
  randomtree.C tools/statistics.C tools/joint-A-T.C tools/partition.C
  io.C) 
target_link_libraries(joint_indels ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(joint_parsimony tools/joint-parsimony.C alignment.C
  alphabet.C sequence.C util.C rng.C tree.C sequencetree.C tree-util.C
  setup.C sequence-format.C alignment-random.C alignment-util.C
  randomtree.C tools/parsimony.C tools/joint-A-T.C n_indels.C io.C) 
target_link_libraries(joint_parsimony ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_info tools/alignment-info.C alignment.C
  alphabet.C sequence.C util.C rng.C tree.C sequencetree.C setup.C
  tools/parsimony.C sequence-format.C randomtree.C alignment-util.C
  alignment-random.C tree-util.C tools/statistics.C io.C)
target_link_libraries(alignment_info ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_cat tools/alignment-cat.C alphabet.C
  sequence.C util.C sequence-format.C io.C) 
target_link_libraries(alignment_cat ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_translate tools/alignment-translate.C
  alignment.C alignment-util.C alphabet.C sequence.C sequence-format.C
  util.C tree.C setup.C sequencetree.C randomtree.C rng.C tree-util.C
  alignment-random.C io.C) 
target_link_libraries(alignment_translate ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_find tools/alignment-find.C alignment.C
  alphabet.C sequence.C alignment-util.C rng.C util.C
  sequence-format.C tree.C io.C sequencetree.C) 
target_link_libraries(alignment_find ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_convert tools/alignment-convert.C alignment.C
  alignment-util.C sequence.C alphabet.C util.C sequence-format.C
  tree.C io.C sequencetree.C) 
target_link_libraries(alignment_convert ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(alignment_find_conserved
  tools/alignment-find-conserved.C alignment.C alphabet.C sequence.C
  util.C rng.C tree.C sequencetree.C setup.C tools/parsimony.C
  sequence-format.C randomtree.C alignment-util.C alignment-random.C
  tree-util.C tools/statistics.C tools/partition.C io.C)
target_link_libraries(alignment_find_conserved ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(trees_consensus tools/trees-consensus.C tree.C
  sequencetree.C tools/tree-dist.C util.C tools/statistics.C
  tree-util.C tools/mctree.C rng.C  tools/partition.C
  tools/consensus-tree.C io.C) 
target_link_libraries(trees_consensus ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(trees_bootstrap tools/trees-bootstrap.C tree.C
  sequencetree.C tools/tree-dist.C util.C rng.C tools/statistics.C
  tools/bootstrap.C tree-util.C  tools/partition.C
  tools/consensus-tree.C io.C) 
target_link_libraries(trees_bootstrap ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(partitions_supported tools/partitions-supported.C
  tree.C sequencetree.C tools/tree-dist.C util.C  tools/statistics.C
  tree-util.C  tools/partition.C io.C) 
target_link_libraries(partitions_supported ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(draw_graph tools/draw-graph.C tree.C sequencetree.C
  tools/tree-dist.C util.C tree-util.C tools/mctree.C rng.C
  tools/partition.C io.C) 
target_link_libraries(draw_graph ${BOOST_LIBS})

#---------------------------------------------------------------
add_executable(tree_mean_lengths tools/tree-mean-lengths.C util.C
  tree.C sequencetree.C tools/tree-dist.C tools/statistics.C
  tree-util.C  tools/partition.C io.C) 
target_link_libraries(tree_mean_lengths ${BOOST_LIBS})

#---------------------------------------------------------------
add_executable(mctree_mean_lengths tools/mctree-mean-lengths.C util.C
  tree.C sequencetree.C tools/tree-dist.C tools/statistics.C
  tree-util.C tools/mctree.C rng.C tools/partition.C io.C) 
target_link_libraries(mctree_mean_lengths ${BOOST_LIBS})

#---------------------------------------------------------------
add_executable(trees_pair_distances tools/trees-pair-distances.C
  util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C
  tree-util.C  tools/partition.C io.C) 
target_link_libraries(trees_pair_distances ${BOOST_LIBS})

#---------------------------------------------------------------
add_executable(tree_partitions tools/tree-partitions.C util.C tree.C
  sequencetree.C tools/tree-dist.C tree-util.C  tools/partition.C
  io.C) 
target_link_libraries(tree_partitions ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(trees_to_SRQ tools/trees-to-SRQ.C util.C tree.C
  sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C
  tools/partition.C io.C) 
target_link_libraries(trees_to_SRQ ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(tree_reroot tools/tree-reroot.C tree.C sequencetree.C
  tree-util.C util.C tools/tree-dist.C tools/partition.C io.C) 
target_link_libraries(tree_reroot  ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(pickout tools/pickout.C util.C)
target_link_libraries(pickout ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(subsample tools/subsample.C)
target_link_libraries(subsample ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(cut_range tools/cut-range.C util.C)
target_link_libraries(cut_range ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(analyze_distances tools/analyze_distances.C alignment.C alphabet.C
	sequence.C tools/distance-methods.C
	util.C sequencetree.C substitution.C eigenvalue.C tree.C
	exponential.C setup-smodel.C smodel/smodel.C imodel.C rng.C likelihood.C
	choose.C tools/optimize.C setup.C rates.C matcache.C alignment-util.C
	sequence-format.C randomtree.C model.C  probability.C
	substitution-cache.C substitution-index.C substitution-star.C tree-util.C
	alignment-random.C parameters.C myexception.C monitor.C
	tools/tree-dist.C tools/inverse.C distribution.C tools/partition.C
	timer_stack.C io.C operator.C expression.C smodel/objects.C context.C
	formula.C computation.C formula_expression.C smodel/operations.C
	distribution-operations.C 2way.C operations.C)
target_link_libraries(analyze_distances ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(trees_distances tools/trees-distances.C tree.C
	sequencetree.C tools/tree-dist.C tools/partition.C util.C
	tree-util.C tools/statistics.C io.C tools/distance-report.C)
target_link_libraries(trees_distances ${BOOST_LIBS})

#---------------------------------------------------------------

#add_executable(draw_tree tools/draw-tree.C tree.C sequencetree.C
#	tools/tree-dist.C util.C tree-util.C tools/mctree.C rng.C
#	util-random.C tools/partition.C io.C)
#target_link_libraries( ${BOOST_LIBS})


#---------------------------------------------------------------

add_executable(path_graph tools/path-graph.C alignment.C alphabet.C
  sequence.C util.C sequence-format.C alignment-util.C tree.C io.C sequencetree.C)
target_link_libraries(path_graph ${BOOST_LIBS})

#---------------------------------------------------------------

add_executable(expression2 expression2.C util.C formula.C context.C
  computation.C operator.C expression.C probability.C
  distribution-operations.C operations.C graph_register.C)
target_link_libraries(expression2 ${BOOST_LIBS})
