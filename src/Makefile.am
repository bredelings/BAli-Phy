noinst_HEADERS = 2way.H dp-engine.H myexception.H sequencetree.H 3way.H \
           dp-matrix.H mytypes.H setup.H 5way.H eigenvalue.H \
           n_indels.H smodel.H alignment-constraint.H exponential.H \
           parameters.H substitution-cache.H alignment.H hmm.H pow2.H \
           substitution.H alignment-sums.H imodel.H probability.H \
           substitution-index.H alignment-util.H likelihood.H proposals.H \
           tree-branchnode.H alphabet.H log-double.H rates.H tree.H \
           bits.H logsum.H tree-util.H choose.H matcache.H  \
           rng.H util.H clone.H mcmc.H sample.H util-random.H \
           model.H sequence-format.H dp-array.H monitor.H sequence.H \
           tools/bootstrap.H tools/inverse.H tools/statistics.H \
           tools/colors.H tools/joint-A-T.H  tools/stats-table.H \
           tools/distance-methods.H tools/optimize.H tools/tree-dist.H \
           tools/findroot.H tools/parsimony.H distribution.H tools/mctree.H \
           version.H cow-ptr.H tools/index-matrix.H cached_value.H

LDFLAGS = @ldflags@

INCLUDES = -I@top_srcdir@/include

if GCC

CXXFLAGS += -pedantic
CXXFLAGS += -Wall -Wextra -Wno-sign-compare -Woverloaded-virtual -Wstrict-aliasing

endif

LIBS = @libs@

#-----------------------------------------------------------------

TOOLS = model_P statreport alignment-gild \
	alignment-reorder alignment-draw alignment-find \
	trees-to-SRQ tree-reroot alignment-chop-internal \
	alignment-convert alignment-thin \
	trees-consensus cut-range alignment-translate \
	alignment-median alignment-indices pickout \
	joint-indels alignment-consensus alignment-info \
	subsample tree-mean-lengths mctree-mean-lengths trees-bootstrap \
	joint-parsimony stats-merge stats-select alignment-identity \
	analyze_distances alignment-cat draw-graph \
	alignment-compare tree-partitions trees-distances \
	partitions-supported trees-pair-distances analyze-rates \
	path-graph alignment-find-conserved alignment-max alignments-diff

bin_PROGRAMS = bali-phy ${TOOLS}

if HAVE_CAIRO
bin_PROGRAMS += draw-tree
endif

#-----------------------------------------------------------------

bali_phy_SOURCES = sequence.C tree.C alignment.C substitution.C moves.C \
          rng.C exponential.C eigenvalue.C parameters.C likelihood.C mcmc.C \
	  choose.C sequencetree.C sample-branch-lengths.C \
	  util.C randomtree.C alphabet.C smodel.C bali-phy.C \
	  hmm.C dp-engine.C dp-array.C dp-matrix.C 3way.C 2way.C sample-alignment.C \
	  sample-tri.C sample-node.C imodel.C 5way.C sample-topology-NNI.C \
	  setup.C rates.C matcache.C sample-two-nodes.C sequence-format.C \
	  util-random.C alignment-random.C setup-smodel.C sample-topology-SPR.C \
	  alignment-sums.C alignment-util.C probability.C model.C \
	  alignment-constraint.C substitution-cache.C substitution-star.C \
	  monitor.C substitution-index.C tree-util.C myexception.C pow2.C \
	  tools/tree-dist.C proposals.C n_indels.C distribution.C \
	  tools/parsimony.C version.C

bali_phy_CXXFLAGS = @MPI_CXXFLAGS@
bali_phy_LDADD = @MPI_LDFLAGS@ @BOOST_MPI_LIBS@

#-----------------------------------------------------------------

model_P_SOURCES = tools/model_P.C tools/statistics.C rng.C

#-------------------------- statreport --------------------------

statreport_SOURCES = tools/statreport.C tools/statistics.C util.C tools/stats-table.C

#-------------------------- statreport --------------------------

stats_merge_SOURCES = tools/stats-merge.C util.C

#-------------------------- statreport --------------------------

stats_select_SOURCES = tools/stats-select.C util.C tools/stats-table.C

#-------------------------- statreport --------------------------

analyze_rates_SOURCES = tools/analyze-rates.C util.C tools/stats-table.C \
	tools/statistics.C

#---------------------------------------------------------------

alignment_gild_SOURCES = tools/alignment-gild.C alignment.C alphabet.C \
	sequence.C util.C rng.C tree.C sequencetree.C tools/optimize.C \
	tools/findroot.C setup.C imodel.C probability.C sequence-format.C \
	model.C tools/distance-methods.C alignment-random.C alignment-util.C \
	randomtree.C tree-util.C tools/inverse.C

alignment_gild_LDADD = ${ATLAS_LIBS}

#---------------------------------------------------------------

alignment_median_SOURCES = tools/alignment-median.C alignment.C alphabet.C sequence.C util.C \
	tree.C sequencetree.C sequence-format.C alignment-util.C 

#---------------------------------------------------------------

alignment_consensus_SOURCES = tools/alignment-consensus.C alignment.C alphabet.C sequence.C util.C rng.C \
	tree.C sequencetree.C util-random.C tools/statistics.C \
	sequence-format.C alignment-util.C tools/index-matrix.C

#---------------------------------------------------------------

alignment_max_SOURCES = tools/alignment-max.C alignment.C alphabet.C sequence.C util.C rng.C \
	tree.C sequencetree.C util-random.C tools/statistics.C \
	sequence-format.C alignment-util.C tools/index-matrix.C

#---------------------------------------------------------------

alignment_compare_SOURCES = tools/alignment-compare.C alignment.C alphabet.C sequence.C util.C rng.C \
	tree.C sequencetree.C util-random.C \
	sequence-format.C alignment-util.C 

#---------------------------------------------------------------

alignment_identity_SOURCES = tools/alignment-identity.C alignment.C alphabet.C sequence.C util.C rng.C \
	tree.C sequencetree.C util-random.C tools/statistics.C \
	sequence-format.C alignment-util.C tools/index-matrix.C

#---------------------------------------------------------------

alignment_reorder_SOURCES = tools/alignment-reorder.C alignment.C alphabet.C sequence.C util.C rng.C \
	tree.C sequencetree.C tools/optimize.C tools/findroot.C setup.C imodel.C \
	sequence-format.C randomtree.C alignment-util.C probability.C alignment-random.C \
	model.C tree-util.C 

#---------------------------------------------------------------

alignment_thin_SOURCES = tools/alignment-thin.C alignment.C alphabet.C sequence.C util.C rng.C \
	tree.C sequencetree.C setup.C imodel.C sequence-format.C randomtree.C \
	alignment-util.C probability.C alignment-random.C model.C tree-util.C \
	tools/distance-methods.C tools/inverse.C

#---------------------------------------------------------------

alignment_chop_internal_SOURCES = tools/alignment-chop-internal.C alignment.C alphabet.C sequence.C util.C tree.C \
	sequence-format.C alignment-util.C 

#---------------------------------------------------------------

alignment_indices_SOURCES = tools/alignment-indices.C alignment.C alphabet.C sequence.C util.C tree.C sequence-format.C alignment-util.C 

#---------------------------------------------------------------

alignments_diff_SOURCES = tools/alignments-diff.C alignment.C alphabet.C sequence.C util.C tree.C sequence-format.C alignment-util.C 

#---------------------------------------------------------------

alignment_draw_SOURCES = tools/alignment-draw.C alignment.C alphabet.C sequence.C sequence-format.C util.C alignment-util.C tools/colors.C tree.C   

#---------------------------------------------------------------

joint_indels_SOURCES = tools/joint-indels.C alignment.C alphabet.C sequence.C util.C rng.C tree.C sequencetree.C tree-util.C setup.C imodel.C probability.C sequence-format.C model.C alignment-random.C alignment-util.C randomtree.C tools/tree-dist.C tools/statistics.C tools/joint-A-T.C 

#---------------------------------------------------------------

joint_parsimony_SOURCES = tools/joint-parsimony.C alignment.C alphabet.C sequence.C util.C rng.C tree.C \
	sequencetree.C tree-util.C setup.C imodel.C probability.C sequence-format.C \
	model.C alignment-random.C alignment-util.C randomtree.C \
	tools/parsimony.C tools/joint-A-T.C n_indels.C

#---------------------------------------------------------------

alignment_info_SOURCES = tools/alignment-info.C alignment.C alphabet.C sequence.C util.C rng.C tree.C sequencetree.C setup.C imodel.C tools/parsimony.C sequence-format.C randomtree.C alignment-util.C probability.C alignment-random.C model.C tree-util.C tools/statistics.C

#---------------------------------------------------------------

alignment_cat_SOURCES = tools/alignment-cat.C alphabet.C sequence.C util.C sequence-format.C

#---------------------------------------------------------------

alignment_translate_SOURCES = tools/alignment-translate.C alignment.C alignment-util.C alphabet.C sequence.C sequence-format.C util.C tree.C setup.C imodel.C model.C probability.C sequencetree.C randomtree.C rng.C tree-util.C alignment-random.C

#---------------------------------------------------------------

alignment_find_SOURCES = tools/alignment-find.C alignment.C alphabet.C sequence.C alignment-util.C rng.C util.C sequence-format.C tree.C 

#---------------------------------------------------------------

alignment_convert_SOURCES = tools/alignment-convert.C alignment.C alignment-util.C sequence.C alphabet.C util.C sequence-format.C tree.C 

#---------------------------------------------------------------

alignment_find_conserved_SOURCES = tools/alignment-find-conserved.C alignment.C alphabet.C sequence.C util.C rng.C tree.C sequencetree.C setup.C imodel.C tools/parsimony.C sequence-format.C randomtree.C alignment-util.C probability.C alignment-random.C model.C tree-util.C tools/statistics.C tools/tree-dist.C

#---------------------------------------------------------------

trees_consensus_SOURCES = tools/trees-consensus.C tree.C sequencetree.C tools/tree-dist.C util.C tools/statistics.C tree-util.C

#---------------------------------------------------------------

trees_bootstrap_SOURCES = tools/trees-bootstrap.C tree.C sequencetree.C tools/tree-dist.C util.C rng.C tools/statistics.C tools/bootstrap.C tree-util.C

#---------------------------------------------------------------

partitions_supported_SOURCES = tools/partitions-supported.C tree.C sequencetree.C tools/tree-dist.C util.C  tools/statistics.C tree-util.C

#---------------------------------------------------------------

draw_graph_SOURCES = tools/draw-graph.C tree.C sequencetree.C tools/tree-dist.C util.C tree-util.C tools/mctree.C

#---------------------------------------------------------------
tree_mean_lengths_SOURCES = tools/tree-mean-lengths.C util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C 

#---------------------------------------------------------------
mctree_mean_lengths_SOURCES = tools/mctree-mean-lengths.C util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C tools/mctree.C

#---------------------------------------------------------------
trees_pair_distances_SOURCES = tools/trees-pair-distances.C util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C 

#---------------------------------------------------------------
tree_partitions_SOURCES = tools/tree-partitions.C util.C tree.C sequencetree.C tools/tree-dist.C tree-util.C 

#---------------------------------------------------------------

trees_to_SRQ_SOURCES = tools/trees-to-SRQ.C util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C

#---------------------------------------------------------------

tree_reroot_SOURCES = tools/tree-reroot.C tree.C sequencetree.C tree-util.C util.C tools/tree-dist.C

#---------------------------------------------------------------

pickout_SOURCES = tools/pickout.C util.C

#---------------------------------------------------------------

subsample_SOURCES = tools/subsample.C

#---------------------------------------------------------------

cut_range_SOURCES = tools/cut-range.C util.C

#---------------------------------------------------------------

analyze_distances_SOURCES = tools/analyze_distances.C alignment.C alphabet.C \
	sequence.C tools/distance-methods.C \
	util.C sequencetree.C substitution.C eigenvalue.C tree.C \
	exponential.C setup-smodel.C smodel.C imodel.C rng.C likelihood.C \
	choose.C tools/optimize.C setup.C rates.C matcache.C alignment-util.C \
	sequence-format.C randomtree.C model.C  probability.C \
	substitution-cache.C substitution-index.C substitution-star.C tree-util.C \
	alignment-random.C parameters.C myexception.C monitor.C \
	tools/tree-dist.C tools/inverse.C distribution.C

#---------------------------------------------------------------

trees_distances_SOURCES = tools/trees-distances.C tree.C \
	sequencetree.C tools/tree-dist.C util.C tree-util.C tools/statistics.C

#---------------------------------------------------------------

draw_tree_SOURCES = tools/draw-tree.C tree.C sequencetree.C \
	tools/tree-dist.C util.C tree-util.C tools/mctree.C rng.C util-random.C
draw_tree_CXXFLAGS = ${CAIRO_CFLAGS}
draw_tree_LDADD = ${CAIRO_LIBS}

#---------------------------------------------------------------

path_graph_SOURCES = tools/path-graph.C alignment.C alphabet.C sequence.C util.C \
	sequence-format.C alignment-util.C tree.C

#---------------------------------------------------------------

alignment_cut: alignment.o alignment-util.o alphabet.o sequence.o \
	sequence-format.o util.o tree.o ${BOOST_LIBS}

#---------------------------------------------------------------

tree_dist_autocorrelation: tree.o sequencetree.o tools/tree-dist.o

#---------------------------------------------------------------

tree_dist_cvars: tree.o sequencetree.o util.o tools/tree-dist.o

#---------------------------------------------------------------

srq_analyze: rng.o tools/statistics.o ${LIBS}

#---------------------------------------------------------------

make_random_tree: tree.o sequencetree.o util.o rng.o ${LIBS}

#---------------------------------------------------------------

test: tools/optimize.o

#---------------------------------------------------------------

test_smodel: alignment.o alphabet.o sequence.o tree.o sequencetree.o util.o \
	setup-smodel.o smodel.o randomtree.o model.o sequence-format.o rates.o \
	probability.o rng.o setup-smodel.o exponential.o eigenvalue.o \
	alignment-util.o setup.o imodel.o alignment-random.o ${LIBS}

#---------------------------------------------------------------

truckgraph: alignment.o alphabet.o sequence.o util.o rng.o ${LIBS}

#---------------------------------------------------------------

truckgraph3d: alignment.o alphabet.o sequence.o util.o rng.o 

