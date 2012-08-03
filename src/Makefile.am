SED = sed
GIT_VERSION_CMD = $(SHELL) $(top_srcdir)/scripts/git_version.sh
git_version.stamp:
	@if test -f "$(srcdir)/git_version.h"; then \
		if test -f "git_version.h"; then :; \
		else \
			cp "$(srcdir)/git_version.h" "git_version.h"; \
		fi; \
	fi
	$(GIT_VERSION_CMD) -k -s $(top_srcdir) -o git_version.h
	@if test -s "$(srcdir)/git_version.h"; then \
		if cmp "$(srcdir)/git_version.h" "git_version.h"; then :; \
		else \
			echo "Error: $(srcdir)/git_version.h and git_version.h differ."; \
			echo "       You probably want to remove the former."; \
			exit 1; \
		fi; \
	fi

dist-hook: git_version.stamp
	if test -f "git_version.h"; then \
		$(SED) -e 's|^#undef GIT_IS_DIST.*|#define GIT_IS_DIST 1|' \
			"git_version.h" > "$(distdir)/git_version.h"; \
	fi

#-----------------------------------------------------------------

noinst_HEADERS = dp/2way.H dp/dp-engine.H myexception.H sequencetree.H dp/3way.H \
           dp/dp-matrix.H mytypes.H setup.H dp/5way.H eigenvalue.H \
           n_indels.H smodel/smodel.H alignment-constraint.H exponential.H \
           parameters.H substitution/substitution-cache.H alignment/alignment.H dp/hmm.H pow2.H \
           substitution/substitution.H dp/alignment-sums.H imodel.H probability.H \
           substitution/substitution-index.H alignment/alignment-util.H likelihood.H mcmc/proposals.H \
           tree-branchnode.H sequence/alphabet.H log-double.H smodel/rates.H tree.H \
           dp/bits.H logsum.H tree-util.H choose.H substitution/matcache.H  \
           rng.H util.H object.H mcmc/mcmc.H mcmc/sample.H util-random.H \
           model.H sequence/sequence-format.H dp/dp-array.H monitor.H sequence/sequence.H \
           tools/bootstrap.H tools/inverse.H tools/statistics.H \
           tools/colors.H tools/joint-A-T.H  tools/stats-table.H \
           tools/distance-methods.H tools/optimize.H tools/tree-dist.H \
           tools/findroot.H tools/parsimony.H distribution.H tools/mctree.H \
           version.H cow-ptr.H tools/index-matrix.H cached_value.H \
	   tools/consensus-tree.H tools/partition.H mcmc/slice-sampling.H \
	   timer_stack.H mcmc/setup.H probability-model.H owned-ptr.H \
	   bounds.H io.H smodel/objects.H smodel/operations.H

LDFLAGS = @ldflags@

INCLUDES = -I@top_srcdir@/include

if GCC

#CXXFLAGS += -pedantic @MPI_CXXFLAGS@ ${CAIRO_CFLAGS}
CXXFLAGS += -pedantic ${CAIRO_CFLAGS}
CXXFLAGS += -Wall -Wextra -Wno-sign-compare -Woverloaded-virtual -Wstrict-aliasing

endif

LIBS = @libs@
if NO_SYSTEM_BOOST
LIBS += ../boost/lib/libboost_myparts.la
endif


#-----------------------------------------------------------------

TOOLS = model_P statreport alignment-gild \
	alignment-draw alignment-find \
	trees-to-SRQ tree-reroot alignment-chop-internal \
	alignment-convert alignment-thin \
	trees-consensus cut-range alignment-translate \
	alignment-median alignment-indices pickout \
	joint-indels alignment-consensus alignment-info \
	subsample tree-mean-lengths mctree-mean-lengths trees-bootstrap \
	joint-parsimony stats-merge stats-select alignment-identity \
	alignment-cat draw-graph \
	alignment-compare tree-partitions trees-distances \
	partitions-supported trees-pair-distances analyze-rates \
	path-graph alignment-find-conserved alignment-max alignments-diff \
	stats-cat expression2

bin_PROGRAMS = bali-phy ${TOOLS}

if HAVE_CAIRO
bin_PROGRAMS += draw-tree	
endif

#-----------------------------------------------------------------

bali_phy_SOURCES = sequence/sequence.C tree.C alignment/alignment.C substitution/substitution.C mcmc/moves.C \
          rng.C exponential.C eigenvalue.C parameters.C likelihood.C mcmc/mcmc.C \
	  choose.C sequencetree.C mcmc/sample-branch-lengths.C \
	  util.C randomtree.C sequence/alphabet.C smodel/smodel.C bali-phy.C \
	  dp/hmm.C dp/dp-engine.C dp/dp-array.C dp/dp-matrix.C dp/3way.C dp/2way.C mcmc/sample-alignment.C \
	  mcmc/sample-tri.C mcmc/sample-node.C imodel.C dp/5way.C mcmc/sample-topology-NNI.C \
	  setup.C smodel/rates.C mcmc/sample-two-nodes.C sequence/sequence-format.C \
	  util-random.C alignment-random.C smodel/setup.C mcmc/sample-topology-SPR.C \
	  dp/alignment-sums.C alignment/alignment-util.C probability.C model.C \
	  alignment-constraint.C substitution/substitution-cache.C \
	  monitor.C substitution/substitution-index.C tree-util.C myexception.C pow2.C \
	  tools/partition.C mcmc/proposals.C n_indels.C distribution.C \
	  tools/parsimony.C version.C mcmc/slice-sampling.C timer_stack.C \
	  mcmc/setup.C io.C mcmc/logger.C mcmc/AIS.C computation/operator.C computation/expression.C \
	  setup-imodel.C smodel/objects.C computation/computation.C \
	  computation/graph_register.C computation/program.C \
	  computation/formula_expression.C smodel/operations.C distribution-operations.C \
	  computation/operations.C computation/prelude.C computation/context.C \
	  computation/model_notes.C substitution/matcache.C smodel/functions.C computation/closure.C \
	  computation/let-float.C BUGS.C dp/dp_hmm.C

nodist_bali_phy_SOURCES = git_version.h

if WITH_MPI
bali_phy_LDADD = ${LDADD} @MPI_LDFLAGS@ 
if NO_SYSTEM_BOOST
bali_phy_LDADD += ../boost/lib/libboost_myparts_mpi.la
else
bali_phy_LDADD += ${LIBS} -lboost_mpi${BOOST_SUFFIX} -lboost_serialization${BOOST_SUFFIX}
endif
endif

# always "rebuild" these
BUILT_SOURCES = version.C git_version.stamp

#-----------------------------------------------------------------

model_P_SOURCES = tools/model_P.C tools/statistics.C rng.C util.C

#-------------------------- statreport --------------------------

statreport_SOURCES = tools/statreport.C tools/statistics.C util.C tools/stats-table.C io.C

#-------------------------- statreport --------------------------

stats_merge_SOURCES = tools/stats-merge.C util.C io.C

#-------------------------- statreport --------------------------

stats_select_SOURCES = tools/stats-select.C util.C tools/stats-table.C io.C

#-------------------------- statreport --------------------------

stats_cat_SOURCES = tools/stats-cat.C util.C tools/stats-table.C io.C

#-------------------------- statreport --------------------------

analyze_rates_SOURCES = tools/analyze-rates.C util.C tools/stats-table.C \
	tools/statistics.C io.C

#---------------------------------------------------------------

alignment_gild_SOURCES = tools/alignment-gild.C alignment/alignment.C sequence/alphabet.C \
	sequence/sequence.C util.C rng.C tree.C sequencetree.C tools/optimize.C \
	tools/findroot.C setup.C sequence/sequence-format.C \
	tools/distance-methods.C alignment-random.C alignment/alignment-util.C \
	randomtree.C tree-util.C tools/inverse.C io.C

alignment_gild_LDADD = ${ATLAS_LIBS}

#---------------------------------------------------------------

alignment_median_SOURCES = tools/alignment-median.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C \
	tree.C sequencetree.C sequence/sequence-format.C alignment/alignment-util.C io.C tools/distance-report.C tools/statistics.C

#---------------------------------------------------------------

alignment_consensus_SOURCES = tools/alignment-consensus.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C \
	tree.C sequencetree.C util-random.C tools/statistics.C \
	sequence/sequence-format.C alignment/alignment-util.C tools/index-matrix.C io.C

#---------------------------------------------------------------

alignment_max_SOURCES = tools/alignment-max.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C \
	tree.C sequencetree.C util-random.C tools/statistics.C \
	sequence/sequence-format.C alignment/alignment-util.C tools/index-matrix.C io.C

#---------------------------------------------------------------

alignment_compare_SOURCES = tools/alignment-compare.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C \
	tree.C sequencetree.C util-random.C \
	sequence/sequence-format.C alignment/alignment-util.C io.C

#---------------------------------------------------------------

alignment_identity_SOURCES = tools/alignment-identity.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C \
	tree.C sequencetree.C util-random.C tools/statistics.C \
	sequence/sequence-format.C alignment/alignment-util.C tools/index-matrix.C io.C

#---------------------------------------------------------------

alignment_thin_SOURCES = tools/alignment-thin.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C \
	tree.C sequencetree.C setup.C sequence/sequence-format.C randomtree.C \
	alignment/alignment-util.C alignment-random.C tree-util.C \
	tools/distance-methods.C tools/inverse.C tools/index-matrix.C io.C

#---------------------------------------------------------------

alignment_chop_internal_SOURCES = tools/alignment-chop-internal.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C tree.C \
	sequence/sequence-format.C alignment/alignment-util.C io.C sequencetree.C

#---------------------------------------------------------------

alignment_indices_SOURCES = tools/alignment-indices.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C tree.C sequence/sequence-format.C alignment/alignment-util.C io.C sequencetree.C

#---------------------------------------------------------------

alignments_diff_SOURCES = tools/alignments-diff.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C tree.C sequence/sequence-format.C alignment/alignment-util.C io.C sequencetree.C

#---------------------------------------------------------------

alignment_draw_SOURCES = tools/alignment-draw.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C sequence/sequence-format.C util.C alignment/alignment-util.C tools/colors.C tree.C io.C sequencetree.C

#---------------------------------------------------------------

joint_indels_SOURCES = tools/joint-indels.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C tree.C sequencetree.C tree-util.C setup.C sequence/sequence-format.C alignment-random.C alignment/alignment-util.C randomtree.C tools/statistics.C tools/joint-A-T.C tools/partition.C io.C

#---------------------------------------------------------------

joint_parsimony_SOURCES = tools/joint-parsimony.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C tree.C \
	sequencetree.C tree-util.C setup.C sequence/sequence-format.C \
	alignment-random.C alignment/alignment-util.C randomtree.C \
	tools/parsimony.C tools/joint-A-T.C n_indels.C io.C

#---------------------------------------------------------------

alignment_info_SOURCES = tools/alignment-info.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C tree.C sequencetree.C setup.C tools/parsimony.C sequence/sequence-format.C randomtree.C alignment/alignment-util.C alignment-random.C tree-util.C tools/statistics.C io.C

#---------------------------------------------------------------

alignment_cat_SOURCES = tools/alignment-cat.C sequence/alphabet.C sequence/sequence.C util.C sequence/sequence-format.C io.C \
	tree.C sequencetree.C tools/optimize.C tools/findroot.C 

#---------------------------------------------------------------

alignment_translate_SOURCES = tools/alignment-translate.C alignment/alignment.C alignment/alignment-util.C sequence/alphabet.C sequence/sequence.C sequence/sequence-format.C util.C tree.C setup.C sequencetree.C randomtree.C rng.C tree-util.C alignment-random.C io.C

#---------------------------------------------------------------

alignment_find_SOURCES = tools/alignment-find.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C alignment/alignment-util.C rng.C util.C sequence/sequence-format.C tree.C io.C sequencetree.C

#---------------------------------------------------------------

alignment_convert_SOURCES = tools/alignment-convert.C alignment/alignment.C alignment/alignment-util.C sequence/sequence.C sequence/alphabet.C util.C sequence/sequence-format.C tree.C io.C sequencetree.C

#---------------------------------------------------------------

alignment_find_conserved_SOURCES = tools/alignment-find-conserved.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C tree.C sequencetree.C setup.C tools/parsimony.C sequence/sequence-format.C randomtree.C alignment/alignment-util.C alignment-random.C tree-util.C tools/statistics.C tools/partition.C io.C

#---------------------------------------------------------------

trees_consensus_SOURCES = tools/trees-consensus.C tree.C sequencetree.C tools/tree-dist.C util.C tools/statistics.C tree-util.C tools/mctree.C rng.C  tools/partition.C tools/consensus-tree.C io.C

#---------------------------------------------------------------

trees_bootstrap_SOURCES = tools/trees-bootstrap.C tree.C sequencetree.C tools/tree-dist.C util.C rng.C tools/statistics.C tools/bootstrap.C tree-util.C  tools/partition.C tools/consensus-tree.C io.C

#---------------------------------------------------------------

partitions_supported_SOURCES = tools/partitions-supported.C tree.C sequencetree.C tools/tree-dist.C util.C  tools/statistics.C tree-util.C  tools/partition.C io.C

#---------------------------------------------------------------

draw_graph_SOURCES = tools/draw-graph.C tree.C sequencetree.C tools/tree-dist.C util.C tree-util.C tools/mctree.C rng.C  tools/partition.C io.C

#---------------------------------------------------------------
tree_mean_lengths_SOURCES = tools/tree-mean-lengths.C util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C  tools/partition.C io.C

#---------------------------------------------------------------
mctree_mean_lengths_SOURCES = tools/mctree-mean-lengths.C util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C tools/mctree.C rng.C tools/partition.C io.C

#---------------------------------------------------------------
trees_pair_distances_SOURCES = tools/trees-pair-distances.C util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C  tools/partition.C io.C

#---------------------------------------------------------------
tree_partitions_SOURCES = tools/tree-partitions.C util.C tree.C sequencetree.C tools/tree-dist.C tree-util.C  tools/partition.C io.C

#---------------------------------------------------------------

trees_to_SRQ_SOURCES = tools/trees-to-SRQ.C util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C tools/partition.C io.C

#---------------------------------------------------------------

tree_reroot_SOURCES = tools/tree-reroot.C tree.C sequencetree.C tree-util.C util.C tools/tree-dist.C tools/partition.C io.C

#---------------------------------------------------------------

pickout_SOURCES = tools/pickout.C util.C

#---------------------------------------------------------------

subsample_SOURCES = tools/subsample.C

#---------------------------------------------------------------

cut_range_SOURCES = tools/cut-range.C util.C

#---------------------------------------------------------------

#analyze_distances_SOURCES = tools/analyze_distances.C alignment/alignment.C sequence/alphabet.C \
#	sequence/sequence.C tools/distance-methods.C \
#	util.C sequencetree.C substitution/substitution.C eigenvalue.C tree.C \
#	exponential.C smodel/setup.C smodel/smodel.C imodel.C rng.C likelihood.C \
#	choose.C tools/optimize.C setup.C smodel/rates.C alignment/alignment-util.C \
#	sequence/sequence-format.C randomtree.C model.C computation/graph_register.C  probability.C \
#	substitution/substitution-cache.C substitution/substitution-index.C substitution/substitution-star.C tree-util.C \
#	alignment-random.C parameters.C myexception.C monitor.C \
#	tools/tree-dist.C tools/inverse.C distribution.C tools/partition.C \
#	timer_stack.C io.C computation/operator.C computation/expression.C smodel/objects.C \
#	computation/computation.C computation/formula_expression.C smodel/operations.C \
#	distribution-operations.C 2way.C computation/operations.C computation/program.C \
#	computation/prelude.C computation/context.C

#---------------------------------------------------------------

trees_distances_SOURCES = tools/trees-distances.C tree.C \
	sequencetree.C tools/tree-dist.C tools/partition.C util.C \
	tree-util.C tools/statistics.C io.C tools/distance-report.C

#---------------------------------------------------------------

draw_tree_SOURCES = tools/draw-tree.C tree.C sequencetree.C \
	tools/tree-dist.C util.C tree-util.C tools/mctree.C rng.C \
	util-random.C tools/partition.C io.C
draw_tree_LDADD = ${CAIRO_LIBS}

#---------------------------------------------------------------

path_graph_SOURCES = tools/path-graph.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C \
	sequence/sequence-format.C alignment/alignment-util.C tree.C io.C sequencetree.C

#---------------------------------------------------------------

expression2_SOURCES = computation/expression2.C util.C computation/computation.C \
	computation/operator.C computation/expression.C probability.C distribution-operations.C \
	computation/operations.C computation/graph_register.C computation/program.C \
	computation/prelude.C computation/context.C eigenvalue.C exponential.C computation/model_notes.C \
	computation/closure.C computation/let-float.C

#---------------------------------------------------------------

alignment_cut: alignment.o alignment-util.o sequence/alphabet.o sequence.o \
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

test_smodel: alignment.o sequence/alphabet.o sequence.o tree.o sequencetree.o util.o \
	setup-smodel.o smodel.o randomtree.o model.o sequence-format.o smodel/rates.o \
	probability.o rng.o setup-smodel.o exponential.o eigenvalue.o \
	alignment-util.o setup.o imodel.o alignment-random.o ${LIBS}

#---------------------------------------------------------------

truckgraph: alignment.o sequence/alphabet.o sequence.o util.o rng.o ${LIBS}

#---------------------------------------------------------------

truckgraph3d: alignment.o sequence/alphabet.o sequence.o util.o rng.o 

