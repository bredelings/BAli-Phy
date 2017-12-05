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

SUBDIRS = . builtins/

#-----------------------------------------------------------------

noinst_HEADERS = dp/2way.H dp/dp-engine.H myexception.H tree/sequencetree.H dp/3way.H \
           dp/dp-matrix.H setup.H dp/5way.H math/eigenvalue.H \
           n_indels.H n_indels2.H alignment/alignment-constraint.H math/exponential.H \
           models/parameters.H substitution/cache.H alignment/alignment.H dp/hmm.H math/pow2.H \
           substitution/substitution.H dp/alignment-sums.H imodel/imodel.H probability/probability.H \
           alignment/alignment-util.H prior.H mcmc/proposals.H \
           tree/tree-branchnode.H sequence/alphabet.H math/log-double.H tree/tree.H \
           dp/bits.H math/logsum.H tree/tree-util.H probability/choose.H math/logprod.H \
           rng.H util.H object.H mcmc/mcmc.H mcmc/sample.H util-random.H \
           models/model.H sequence/sequence-format.H dp/dp-array.H monitor.H sequence/sequence.H \
           tools/bootstrap.H tools/inverse.H tools/statistics.H \
           tools/colors.H tools/joint-A-T.H  tools/stats-table.H \
           tools/distance-methods.H tools/optimize.H tools/tree-dist.H \
           tools/findroot.H tools/parsimony.H tools/parsimony2.H tools/mctree.H \
           version.H cow-ptr.H alignment/index-matrix.H cached_value.H \
	   tools/consensus-tree.H tools/partition.H mcmc/slice-sampling.H \
	   timer_stack.H mcmc/setup.H owned-ptr.H alignment/alignment-util2.H \
	   bounds.H io.H parser/parse.H models/TreeInterface.H \
           computation/computation.H computation/closure.H \
           computation/context.H computation/expression/AST_node.H \
	   computation/expression/expression_ref.H computation/expression/expression.H \
	   computation/expression/apply.H computation/expression/substitute.H \
	   computation/expression/constructor.H computation/model_expression.H \
	   computation/expression/indexify.H computation/expression/let.H \
	   computation/expression/case.H computation/expression/trim.H \
	   computation/expression/tuple.H computation/expression/list.H \
	   computation/bithash.H computation/machine/graph_register.H computation/optimization/let-float.H \
           computation/loader.H computation/module.H computation/expression/dummy.H \
	   computation/operation.H computation/operations.H computation/operator.H \
           computation/machine/pool.H computation/program.H computation/expression/lambda.H \
	   builtins/iota.H computation/machine/CacheList.H mcmc/logger.H models/setup.H \
           dp/dp_hmm.H parser/desugar.H mcmc/AIS.H tools/distance-report.H \
	   matrix.H builtins/vector_from_list.H startup/A-T-model.H startup/files.H startup/loggers.H \
	   startup/io.H startup/system.H startup/cmd_line.H computation/type_constant.H \
	   substitution/parsimony.H models/rules.H models/parse.H models/translate.H \
	   models/unification.H computation/optimization/simplifier.H tools/read-trees.H \
           alignment/load.H computation/optimization/occurrence.H computation/optimization/inliner.H \
	   startup/paths.H

LDFLAGS = @ldflags@ 

AM_CPPFLAGS = @AM_CPPFLAGS@ -I@top_srcdir@/src
AM_CXXFLAGS = @AM_CXXFLAGS@

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
	joint-indels alignment-consensus alignment-info alignment-smc\
	subsample tree-mean-lengths mctree-mean-lengths trees-bootstrap \
	joint-parsimony stats-merge stats-select alignment-identity \
	alignment-cat draw-graph \
	alignment-compare tree-partitions trees-distances \
	partitions-supported trees-pair-distances analyze-rates \
	path-graph alignment-find-conserved alignment-max alignments-diff \
	stats-cat

bin_PROGRAMS = bali-phy ${TOOLS}

if HAVE_CAIRO
bin_PROGRAMS += draw-tree	
endif

#-----------------------------------------------------------------

bali_phy_SOURCES = parser/parse.cc dp/dp_hmm.cc parser/desugar.cc sequence/sequence.cc \
          tree/tree.cc alignment/alignment.cc substitution/substitution.cc mcmc/moves.cc \
          rng.cc math/exponential.cc math/eigenvalue.cc models/parameters.cc prior.cc mcmc/mcmc.cc \
	  probability/choose.cc tree/sequencetree.cc mcmc/sample-branch-lengths.cc \
	  util.cc tree/randomtree.cc sequence/alphabet.cc bali-phy.cc models/TreeInterface.cc \
	  dp/hmm.cc dp/dp-engine.cc dp/dp-array.cc dp/dp-matrix.cc dp/3way.cc dp/2way.cc mcmc/sample-alignment.cc \
	  mcmc/sample-node.cc imodel/imodel.cc dp/5way.cc mcmc/sample-topology-NNI.cc \
	  setup.cc mcmc/sample-two-nodes.cc sequence/sequence-format.cc \
	  util-random.cc alignment/alignment-random.cc models/setup.cc mcmc/sample-topology-SPR.cc \
	  dp/alignment-sums.cc alignment/alignment-util.cc probability/probability.cc models/model.cc \
	  alignment/alignment-constraint.cc monitor.cc \
	  tree/tree-util.cc myexception.cc math/pow2.cc substitution/parsimony.cc \
	  tools/partition.cc mcmc/proposals.cc n_indels.cc n_indels2.cc alignment/alignment-util2.cc \
	  tools/parsimony.cc tools/parsimony2.cc version.cc mcmc/slice-sampling.cc timer_stack.cc \
	  mcmc/setup.cc io.cc mcmc/logger.cc mcmc/AIS.cc computation/operator.cc \
	  computation/expression/expression.cc computation/expression/constructor.cc \
	  computation/expression/expression_ref.cc computation/expression/AST_node.cc \
	  computation/expression/apply.cc computation/expression/substitute.cc \
	  computation/expression/indexify.cc computation/expression/let.cc \
	  computation/expression/case.cc  computation/expression/trim.cc \
	  computation/expression/tuple.cc computation/expression/list.cc \
	  computation/expression/dummy.cc computation/expression/lambda.cc \
	  computation/computation.cc computation/machine/tokens.cc computation/preprocess.cc \
	  computation/machine/graph_register.cc computation/machine/show_graph.cc computation/module.cc \
	  computation/model_expression.cc computation/machine/evaluate.cc computation/machine/gc.cc computation/machine/reroot.cc \
	  computation/operations.cc computation/loader.cc computation/context.cc computation/closure.cc \
	  computation/optimization/let-float.cc computation/program.cc mcmc/sample-tri.cc startup/A-T-model.cc startup/files.cc \
	  startup/loggers.cc startup/system.cc startup/cmd_line.cc models/rules.cc models/parse.cc \
	  models/translate.cc models/unification.cc computation/optimization/simplifier.cc \
	  tools/read-trees.cc alignment/load.cc alignment/index-matrix.cc startup/paths.cc \
	  computation/optimization/occurrence.cc computation/optimization/inliner.cc

if HOST_MINGW32
bali_phy_SOURCES += dlfcn-win32/dlfcn.c
endif

nodist_bali_phy_SOURCES = git_version.h

if HOST_LINUX
bali_phy_LDFLAGS = @ldflags@ -rdynamic
else
if HOST_WIN_ALL
bali_phy_LDFLAGS = @ldflags@ -Wl,--export-all-symbols,--out-implib=libbali-phy.dll.a
else
bali_phy_LDFLAGS = @ldflags@
endif
endif

if WITH_MPI
bali_phy_LDADD = ${LDADD} libsums.a @MPI_LDFLAGS@ 
if NO_SYSTEM_BOOST
bali_phy_LDADD += ../boost/lib/libboost_myparts_mpi.la
else
bali_phy_LDADD += ${LIBS} -lboost_mpi${BOOST_SUFFIX} -lboost_serialization${BOOST_SUFFIX}
endif
else
bali_phy_LDADD = ${LDADD} libsums.a
endif


# always "rebuild" these
BUILT_SOURCES = version.cc git_version.stamp

noinst_LIBRARIES = libsums.a
libsums_a_SOURCES = computation/machine/sums.cc
libsums_a_CXXFLAGS = ${AM_CXXFLAGS} -fno-fast-math
libsums_a_CFLAGS = ${AM_CFLAGS} -fno-fast-math

#-----------------------------------------------------------------

model_P_SOURCES = tools/model_P.cc tools/statistics.cc rng.cc util.cc

#-------------------------- statreport --------------------------

statreport_SOURCES = tools/statreport.cc tools/statistics.cc util.cc tools/stats-table.cc io.cc

#-------------------------- statreport --------------------------

stats_merge_SOURCES = tools/stats-merge.cc util.cc io.cc

#-------------------------- statreport --------------------------

stats_select_SOURCES = tools/stats-select.cc util.cc tools/stats-table.cc io.cc

#-------------------------- statreport --------------------------

stats_cat_SOURCES = tools/stats-cat.cc util.cc tools/stats-table.cc io.cc

#-------------------------- statreport --------------------------

analyze_rates_SOURCES = tools/analyze-rates.cc util.cc tools/stats-table.cc \
	tools/statistics.cc io.cc

#---------------------------------------------------------------

alignment_gild_SOURCES = tools/alignment-gild.cc alignment/alignment.cc sequence/alphabet.cc \
	sequence/sequence.cc util.cc rng.cc tree/tree.cc tree/sequencetree.cc tools/optimize.cc \
	tools/findroot.cc setup.cc sequence/sequence-format.cc \
	tools/distance-methods.cc alignment/alignment-random.cc alignment/alignment-util.cc \
	tree/randomtree.cc tree/tree-util.cc tools/inverse.cc io.cc tools/read-trees.cc \
	alignment/load.cc

alignment_gild_LDADD = ${ATLAS_LIBS}

#---------------------------------------------------------------

alignment_median_SOURCES = tools/alignment-median.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc \
	tree/tree.cc tree/sequencetree.cc sequence/sequence-format.cc alignment/alignment-util.cc io.cc tools/distance-report.cc tools/statistics.cc \
	alignment/load.cc

#---------------------------------------------------------------

alignment_consensus_SOURCES = tools/alignment-consensus.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc rng.cc \
	tree/tree.cc tree/sequencetree.cc util-random.cc tools/statistics.cc \
	sequence/sequence-format.cc alignment/alignment-util.cc alignment/index-matrix.cc io.cc alignment/load.cc

#---------------------------------------------------------------

alignment_max_SOURCES = tools/alignment-max.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc rng.cc \
	tree/tree.cc tree/sequencetree.cc util-random.cc tools/statistics.cc \
	sequence/sequence-format.cc alignment/alignment-util.cc alignment/index-matrix.cc io.cc alignment/load.cc

#---------------------------------------------------------------

alignment_compare_SOURCES = tools/alignment-compare.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc rng.cc \
	tree/tree.cc tree/sequencetree.cc util-random.cc sequence/sequence-format.cc alignment/alignment-util.cc io.cc \
	alignment/load.cc

#---------------------------------------------------------------

alignment_identity_SOURCES = tools/alignment-identity.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc rng.cc \
	tree/tree.cc tree/sequencetree.cc util-random.cc tools/statistics.cc alignment/load.cc \
	sequence/sequence-format.cc alignment/alignment-util.cc alignment/index-matrix.cc io.cc

#---------------------------------------------------------------

alignment_thin_SOURCES = tools/alignment-thin.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc rng.cc \
	tree/tree.cc tree/sequencetree.cc setup.cc sequence/sequence-format.cc tree/randomtree.cc \
	alignment/alignment-util.cc alignment/alignment-random.cc tree/tree-util.cc alignment/load.cc \
	tools/distance-methods.cc tools/inverse.cc alignment/index-matrix.cc io.cc tools/read-trees.cc


#---------------------------------------------------------------

alignment_chop_internal_SOURCES = tools/alignment-chop-internal.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc tree/tree.cc \
	sequence/sequence-format.cc alignment/alignment-util.cc io.cc tree/sequencetree.cc alignment/load.cc tree/tree-util.cc tools/read-trees.cc

#---------------------------------------------------------------

alignment_indices_SOURCES = tools/alignment-indices.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc tree/tree.cc sequence/sequence-format.cc alignment/alignment-util.cc io.cc tree/sequencetree.cc alignment/load.cc

#---------------------------------------------------------------

alignments_diff_SOURCES = tools/alignments-diff.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc tree/tree.cc sequence/sequence-format.cc alignment/alignment-util.cc io.cc tree/sequencetree.cc alignment/load.cc

#---------------------------------------------------------------

alignment_draw_SOURCES = tools/alignment-draw.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc sequence/sequence-format.cc util.cc alignment/alignment-util.cc tools/colors.cc tree/tree.cc io.cc tree/sequencetree.cc alignment/load.cc

#---------------------------------------------------------------

joint_indels_SOURCES = tools/joint-indels.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc rng.cc tree/tree.cc tree/sequencetree.cc tree/tree-util.cc setup.cc sequence/sequence-format.cc alignment/alignment-random.cc alignment/alignment-util.cc tree/randomtree.cc tools/statistics.cc tools/joint-A-T.cc tools/partition.cc io.cc tools/read-trees.cc alignment/load.cc

#---------------------------------------------------------------

joint_parsimony_SOURCES = tools/joint-parsimony.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc rng.cc tree/tree.cc \
	tree/sequencetree.cc tree/tree-util.cc setup.cc sequence/sequence-format.cc \
	alignment/alignment-random.cc alignment/alignment-util.cc tree/randomtree.cc \
	tools/parsimony.cc tools/joint-A-T.cc n_indels.cc io.cc tools/read-trees.cc alignment/load.cc

#---------------------------------------------------------------

alignment_info_SOURCES = tools/alignment-info.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc rng.cc tree/tree.cc tree/sequencetree.cc setup.cc tools/parsimony.cc sequence/sequence-format.cc tree/randomtree.cc alignment/alignment-util.cc alignment/alignment-random.cc tree/tree-util.cc tools/statistics.cc io.cc tools/read-trees.cc alignment/load.cc

#---------------------------------------------------------------

alignment_smc_SOURCES = tools/alignment-smc.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc rng.cc tree/tree.cc tree/sequencetree.cc setup.cc tools/parsimony.cc sequence/sequence-format.cc tree/randomtree.cc alignment/alignment-util.cc alignment/alignment-random.cc tree/tree-util.cc tools/statistics.cc io.cc tools/read-trees.cc alignment/load.cc

#---------------------------------------------------------------

alignment_cat_SOURCES = tools/alignment-cat.cc sequence/alphabet.cc sequence/sequence.cc util.cc sequence/sequence-format.cc io.cc \
	tree/tree.cc tree/sequencetree.cc tools/optimize.cc tools/findroot.cc 

#---------------------------------------------------------------

alignment_translate_SOURCES = tools/alignment-translate.cc alignment/alignment.cc alignment/alignment-util.cc sequence/alphabet.cc sequence/sequence.cc sequence/sequence-format.cc util.cc tree/tree.cc setup.cc tree/sequencetree.cc tree/randomtree.cc rng.cc tree/tree-util.cc alignment/alignment-random.cc io.cc tools/tree-dist.cc tools/partition.cc tools/read-trees.cc alignment/load.cc

#---------------------------------------------------------------

alignment_find_SOURCES = tools/alignment-find.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc alignment/alignment-util.cc rng.cc util.cc sequence/sequence-format.cc tree/tree.cc io.cc tree/sequencetree.cc alignment/load.cc

#---------------------------------------------------------------

alignment_convert_SOURCES = tools/alignment-convert.cc alignment/alignment.cc alignment/alignment-util.cc sequence/sequence.cc sequence/alphabet.cc util.cc sequence/sequence-format.cc tree/tree.cc io.cc tree/sequencetree.cc alignment/load.cc

#---------------------------------------------------------------

alignment_find_conserved_SOURCES = tools/alignment-find-conserved.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc rng.cc tree/tree.cc tree/sequencetree.cc setup.cc tools/parsimony.cc sequence/sequence-format.cc tree/randomtree.cc alignment/alignment-util.cc alignment/alignment-random.cc tree/tree-util.cc tools/statistics.cc tools/partition.cc io.cc tools/read-trees.cc alignment/load.cc

#---------------------------------------------------------------

trees_consensus_SOURCES = tools/trees-consensus.cc tree/tree.cc tree/sequencetree.cc tools/tree-dist.cc util.cc tools/statistics.cc tree/tree-util.cc tools/mctree.cc rng.cc  tools/partition.cc tools/consensus-tree.cc io.cc tools/read-trees.cc

#---------------------------------------------------------------

trees_bootstrap_SOURCES = tools/trees-bootstrap.cc tree/tree.cc tree/sequencetree.cc tools/tree-dist.cc util.cc rng.cc tools/statistics.cc tools/bootstrap.cc tree/tree-util.cc  tools/partition.cc tools/consensus-tree.cc io.cc tools/read-trees.cc

#---------------------------------------------------------------

partitions_supported_SOURCES = tools/partitions-supported.cc tree/tree.cc tree/sequencetree.cc tools/tree-dist.cc util.cc  tools/statistics.cc tree/tree-util.cc  tools/partition.cc io.cc tools/read-trees.cc

#---------------------------------------------------------------

draw_graph_SOURCES = tools/draw-graph.cc tree/tree.cc tree/sequencetree.cc tools/tree-dist.cc util.cc tree/tree-util.cc tools/mctree.cc rng.cc  tools/partition.cc io.cc tools/read-trees.cc

#---------------------------------------------------------------
tree_mean_lengths_SOURCES = tools/tree-mean-lengths.cc util.cc tree/tree.cc tree/sequencetree.cc tools/tree-dist.cc tools/statistics.cc tree/tree-util.cc  tools/partition.cc io.cc tools/read-trees.cc

#---------------------------------------------------------------
mctree_mean_lengths_SOURCES = tools/mctree-mean-lengths.cc util.cc tree/tree.cc tree/sequencetree.cc tools/tree-dist.cc tools/statistics.cc tree/tree-util.cc tools/mctree.cc rng.cc tools/partition.cc io.cc tools/read-trees.cc

#---------------------------------------------------------------
trees_pair_distances_SOURCES = tools/trees-pair-distances.cc util.cc tree/tree.cc tree/sequencetree.cc tools/tree-dist.cc tools/statistics.cc tree/tree-util.cc  tools/partition.cc io.cc tools/read-trees.cc

#---------------------------------------------------------------
tree_partitions_SOURCES = tools/tree-partitions.cc util.cc tree/tree.cc tree/sequencetree.cc tools/tree-dist.cc tree/tree-util.cc  tools/partition.cc io.cc tools/read-trees.cc

#---------------------------------------------------------------

trees_to_SRQ_SOURCES = tools/trees-to-SRQ.cc util.cc tree/tree.cc tree/sequencetree.cc tools/tree-dist.cc tools/read-trees.cc tools/statistics.cc tree/tree-util.cc tools/partition.cc io.cc

#---------------------------------------------------------------

tree_reroot_SOURCES = tools/tree-reroot.cc tree/tree.cc tree/sequencetree.cc tree/tree-util.cc util.cc tools/tree-dist.cc tools/partition.cc io.cc tools/read-trees.cc

#---------------------------------------------------------------

pickout_SOURCES = tools/pickout.cc util.cc io.cc

#---------------------------------------------------------------

subsample_SOURCES = tools/subsample.cc io.cc

#---------------------------------------------------------------

cut_range_SOURCES = tools/cut-range.cc util.cc io.cc

#---------------------------------------------------------------

#analyze_distances_SOURCES = tools/analyze_distances.cc alignment/alignment.cc sequence/alphabet.cc \
#	sequence/sequence.cc tools/distance-methods.cc \
#	util.cc tree/sequencetree.cc substitution/substitution.cc eigenvalue.cc tree/tree.cc \
#	exponential.cc models/setup.cc models/smodel.cc imodel.cc rng.cc prior.cc \
#	choose.cc tools/optimize.cc setup.cc models/rates.cc alignment/alignment-util.cc \
#	sequence/sequence-format.cc tree/randomtree.cc model.cc computation/graph_register.cc  probability.cc \
#	substitution/substitution-cache.cc substitution/substitution-index.cc substitution/substitution-star.cc tree/tree-util.cc \
#	alignment-random.cc parameters.cc myexception.cc monitor.cc \
#	tools/tree-dist.cc tools/inverse.cc distribution.cc tools/partition.cc \
#	timer_stack.cc io.cc computation/operator.cc computation/expression.cc models/objects.cc \
#	computation/computation.cc computation/model_expression.cc models/operations.cc \
#	distribution-operations.cc 2way.cc computation/operations.cc computation/module.cc \
#	computation/loader.cc computation/context.cc

#---------------------------------------------------------------

trees_distances_SOURCES = tools/trees-distances.cc tree/tree.cc \
	tree/sequencetree.cc tools/tree-dist.cc tools/partition.cc util.cc \
	tree/tree-util.cc tools/statistics.cc io.cc tools/distance-report.cc tools/read-trees.cc rng.cc

#---------------------------------------------------------------

draw_tree_SOURCES = tools/draw-tree.cc tree/tree.cc tree/sequencetree.cc \
	tools/tree-dist.cc util.cc tree/tree-util.cc tools/mctree.cc rng.cc \
	util-random.cc tools/partition.cc io.cc tools/read-trees.cc
draw_tree_LDADD = ${CAIRO_LIBS}

#---------------------------------------------------------------

path_graph_SOURCES = tools/path-graph.cc alignment/alignment.cc sequence/alphabet.cc sequence/sequence.cc util.cc \
	sequence/sequence-format.cc alignment/alignment-util.cc tree/tree.cc io.cc tree/sequencetree.cc alignment/load.cc

#---------------------------------------------------------------
