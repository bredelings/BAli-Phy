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

noinst_HEADERS = dp/2way.H dp/dp-engine.H myexception.H tree/sequencetree.H dp/3way.H \
           dp/dp-matrix.H mytypes.H setup.H dp/5way.H math/eigenvalue.H \
           n_indels.H alignment/alignment-constraint.H math/exponential.H \
           models/parameters.H substitution/substitution-cache.H alignment/alignment.H dp/hmm.H math/pow2.H \
           substitution/substitution.H dp/alignment-sums.H imodel/imodel.H probability/probability.H \
           substitution/substitution-index.H alignment/alignment-util.H likelihood.H mcmc/proposals.H \
           tree/tree-branchnode.H sequence/alphabet.H math/log-double.H tree/tree.H \
           dp/bits.H math/logsum.H tree/tree-util.H probability/choose.H substitution/matcache.H  \
           rng.H util.H object.H mcmc/mcmc.H mcmc/sample.H util-random.H \
           models/model.H sequence/sequence-format.H dp/dp-array.H monitor.H sequence/sequence.H \
           tools/bootstrap.H tools/inverse.H tools/statistics.H \
           tools/colors.H tools/joint-A-T.H  tools/stats-table.H \
           tools/distance-methods.H tools/optimize.H tools/tree-dist.H \
           tools/findroot.H tools/parsimony.H tools/mctree.H \
           version.H cow-ptr.H tools/index-matrix.H cached_value.H \
	   tools/consensus-tree.H tools/partition.H mcmc/slice-sampling.H \
	   timer_stack.H mcmc/setup.H models/probability-model.H owned-ptr.H \
	   bounds.H io.H smodel/objects.H smodel/operations.H parser/parse.H \
           parser/AST.H

LDFLAGS = @ldflags@

AM_CPPFLAGS = -I@top_srcdir@/include

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
	stats-cat

bin_PROGRAMS = bali-phy ${TOOLS}

if HAVE_CAIRO
bin_PROGRAMS += draw-tree	
endif

#-----------------------------------------------------------------

bali_phy_SOURCES = sequence/sequence.C tree/tree.C alignment/alignment.C substitution/substitution.C mcmc/moves.C \
          rng.C math/exponential.C math/eigenvalue.C models/parameters.C likelihood.C mcmc/mcmc.C \
	  probability/choose.C tree/sequencetree.C mcmc/sample-branch-lengths.C \
	  util.C tree/randomtree.C sequence/alphabet.C bali-phy.C \
	  dp/hmm.C dp/dp-engine.C dp/dp-array.C dp/dp-matrix.C dp/3way.C dp/2way.C mcmc/sample-alignment.C \
	  mcmc/sample-tri.C mcmc/sample-node.C imodel/imodel.C dp/5way.C mcmc/sample-topology-NNI.C \
	  setup.C mcmc/sample-two-nodes.C sequence/sequence-format.C \
	  util-random.C alignment/alignment-random.C smodel/setup.C mcmc/sample-topology-SPR.C \
	  dp/alignment-sums.C alignment/alignment-util.C probability/probability.C models/model.C \
	  alignment/alignment-constraint.C substitution/substitution-cache.C \
	  monitor.C substitution/substitution-index.C tree/tree-util.C myexception.C math/pow2.C \
	  tools/partition.C mcmc/proposals.C n_indels.C \
	  tools/parsimony.C version.C mcmc/slice-sampling.C timer_stack.C \
	  mcmc/setup.C io.C mcmc/logger.C mcmc/AIS.C computation/operator.C computation/expression.C \
	  imodel/setup.C smodel/objects.C computation/computation.C \
	  computation/graph_register.C computation/module.C \
	  computation/formula_expression.C smodel/operations.C \
	  computation/operations.C computation/loader.C computation/context.C \
	  computation/model_notes.C substitution/matcache.C computation/closure.C \
	  computation/let-float.C parser/parse.C dp/dp_hmm.C parser/desugar.C object.C \
	  parser/AST.C computation/program.C

nodist_bali_phy_SOURCES = git_version.h

bali_phy_LDFLAGS = -rdynamic

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
	sequence/sequence.C util.C rng.C tree/tree.C tree/sequencetree.C tools/optimize.C \
	tools/findroot.C setup.C sequence/sequence-format.C \
	tools/distance-methods.C alignment/alignment-random.C alignment/alignment-util.C \
	tree/randomtree.C tree/tree-util.C tools/inverse.C io.C

alignment_gild_LDADD = ${ATLAS_LIBS}

#---------------------------------------------------------------

alignment_median_SOURCES = tools/alignment-median.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C \
	tree/tree.C tree/sequencetree.C sequence/sequence-format.C alignment/alignment-util.C io.C tools/distance-report.C tools/statistics.C

#---------------------------------------------------------------

alignment_consensus_SOURCES = tools/alignment-consensus.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C \
	tree/tree.C tree/sequencetree.C util-random.C tools/statistics.C \
	sequence/sequence-format.C alignment/alignment-util.C tools/index-matrix.C io.C

#---------------------------------------------------------------

alignment_max_SOURCES = tools/alignment-max.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C \
	tree/tree.C tree/sequencetree.C util-random.C tools/statistics.C \
	sequence/sequence-format.C alignment/alignment-util.C tools/index-matrix.C io.C

#---------------------------------------------------------------

alignment_compare_SOURCES = tools/alignment-compare.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C \
	tree/tree.C tree/sequencetree.C util-random.C \
	sequence/sequence-format.C alignment/alignment-util.C io.C

#---------------------------------------------------------------

alignment_identity_SOURCES = tools/alignment-identity.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C \
	tree/tree.C tree/sequencetree.C util-random.C tools/statistics.C \
	sequence/sequence-format.C alignment/alignment-util.C tools/index-matrix.C io.C

#---------------------------------------------------------------

alignment_thin_SOURCES = tools/alignment-thin.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C \
	tree/tree.C tree/sequencetree.C setup.C sequence/sequence-format.C tree/randomtree.C \
	alignment/alignment-util.C alignment/alignment-random.C tree/tree-util.C \
	tools/distance-methods.C tools/inverse.C tools/index-matrix.C io.C

#---------------------------------------------------------------

alignment_chop_internal_SOURCES = tools/alignment-chop-internal.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C tree/tree.C \
	sequence/sequence-format.C alignment/alignment-util.C io.C tree/sequencetree.C

#---------------------------------------------------------------

alignment_indices_SOURCES = tools/alignment-indices.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C tree/tree.C sequence/sequence-format.C alignment/alignment-util.C io.C tree/sequencetree.C

#---------------------------------------------------------------

alignments_diff_SOURCES = tools/alignments-diff.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C tree/tree.C sequence/sequence-format.C alignment/alignment-util.C io.C tree/sequencetree.C

#---------------------------------------------------------------

alignment_draw_SOURCES = tools/alignment-draw.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C sequence/sequence-format.C util.C alignment/alignment-util.C tools/colors.C tree/tree.C io.C tree/sequencetree.C

#---------------------------------------------------------------

joint_indels_SOURCES = tools/joint-indels.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C tree/tree.C tree/sequencetree.C tree/tree-util.C setup.C sequence/sequence-format.C alignment/alignment-random.C alignment/alignment-util.C tree/randomtree.C tools/statistics.C tools/joint-A-T.C tools/partition.C io.C

#---------------------------------------------------------------

joint_parsimony_SOURCES = tools/joint-parsimony.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C tree/tree.C \
	tree/sequencetree.C tree/tree-util.C setup.C sequence/sequence-format.C \
	alignment/alignment-random.C alignment/alignment-util.C tree/randomtree.C \
	tools/parsimony.C tools/joint-A-T.C n_indels.C io.C

#---------------------------------------------------------------

alignment_info_SOURCES = tools/alignment-info.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C tree/tree.C tree/sequencetree.C setup.C tools/parsimony.C sequence/sequence-format.C tree/randomtree.C alignment/alignment-util.C alignment/alignment-random.C tree/tree-util.C tools/statistics.C io.C

#---------------------------------------------------------------

alignment_cat_SOURCES = tools/alignment-cat.C sequence/alphabet.C sequence/sequence.C util.C sequence/sequence-format.C io.C \
	tree/tree.C tree/sequencetree.C tools/optimize.C tools/findroot.C 

#---------------------------------------------------------------

alignment_translate_SOURCES = tools/alignment-translate.C alignment/alignment.C alignment/alignment-util.C sequence/alphabet.C sequence/sequence.C sequence/sequence-format.C util.C tree/tree.C setup.C tree/sequencetree.C tree/randomtree.C rng.C tree/tree-util.C alignment/alignment-random.C io.C

#---------------------------------------------------------------

alignment_find_SOURCES = tools/alignment-find.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C alignment/alignment-util.C rng.C util.C sequence/sequence-format.C tree/tree.C io.C tree/sequencetree.C

#---------------------------------------------------------------

alignment_convert_SOURCES = tools/alignment-convert.C alignment/alignment.C alignment/alignment-util.C sequence/sequence.C sequence/alphabet.C util.C sequence/sequence-format.C tree/tree.C io.C tree/sequencetree.C

#---------------------------------------------------------------

alignment_find_conserved_SOURCES = tools/alignment-find-conserved.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C rng.C tree/tree.C tree/sequencetree.C setup.C tools/parsimony.C sequence/sequence-format.C tree/randomtree.C alignment/alignment-util.C alignment/alignment-random.C tree/tree-util.C tools/statistics.C tools/partition.C io.C

#---------------------------------------------------------------

trees_consensus_SOURCES = tools/trees-consensus.C tree/tree.C tree/sequencetree.C tools/tree-dist.C util.C tools/statistics.C tree/tree-util.C tools/mctree.C rng.C  tools/partition.C tools/consensus-tree.C io.C

#---------------------------------------------------------------

trees_bootstrap_SOURCES = tools/trees-bootstrap.C tree/tree.C tree/sequencetree.C tools/tree-dist.C util.C rng.C tools/statistics.C tools/bootstrap.C tree/tree-util.C  tools/partition.C tools/consensus-tree.C io.C

#---------------------------------------------------------------

partitions_supported_SOURCES = tools/partitions-supported.C tree/tree.C tree/sequencetree.C tools/tree-dist.C util.C  tools/statistics.C tree/tree-util.C  tools/partition.C io.C

#---------------------------------------------------------------

draw_graph_SOURCES = tools/draw-graph.C tree/tree.C tree/sequencetree.C tools/tree-dist.C util.C tree/tree-util.C tools/mctree.C rng.C  tools/partition.C io.C

#---------------------------------------------------------------
tree_mean_lengths_SOURCES = tools/tree-mean-lengths.C util.C tree/tree.C tree/sequencetree.C tools/tree-dist.C tools/statistics.C tree/tree-util.C  tools/partition.C io.C

#---------------------------------------------------------------
mctree_mean_lengths_SOURCES = tools/mctree-mean-lengths.C util.C tree/tree.C tree/sequencetree.C tools/tree-dist.C tools/statistics.C tree/tree-util.C tools/mctree.C rng.C tools/partition.C io.C

#---------------------------------------------------------------
trees_pair_distances_SOURCES = tools/trees-pair-distances.C util.C tree/tree.C tree/sequencetree.C tools/tree-dist.C tools/statistics.C tree/tree-util.C  tools/partition.C io.C

#---------------------------------------------------------------
tree_partitions_SOURCES = tools/tree-partitions.C util.C tree/tree.C tree/sequencetree.C tools/tree-dist.C tree/tree-util.C  tools/partition.C io.C

#---------------------------------------------------------------

trees_to_SRQ_SOURCES = tools/trees-to-SRQ.C util.C tree/tree.C tree/sequencetree.C tools/tree-dist.C tools/statistics.C tree/tree-util.C tools/partition.C io.C

#---------------------------------------------------------------

tree_reroot_SOURCES = tools/tree-reroot.C tree/tree.C tree/sequencetree.C tree/tree-util.C util.C tools/tree-dist.C tools/partition.C io.C

#---------------------------------------------------------------

pickout_SOURCES = tools/pickout.C util.C

#---------------------------------------------------------------

subsample_SOURCES = tools/subsample.C

#---------------------------------------------------------------

cut_range_SOURCES = tools/cut-range.C util.C

#---------------------------------------------------------------

#analyze_distances_SOURCES = tools/analyze_distances.C alignment/alignment.C sequence/alphabet.C \
#	sequence/sequence.C tools/distance-methods.C \
#	util.C tree/sequencetree.C substitution/substitution.C eigenvalue.C tree/tree.C \
#	exponential.C smodel/setup.C smodel/smodel.C imodel.C rng.C likelihood.C \
#	choose.C tools/optimize.C setup.C smodel/rates.C alignment/alignment-util.C \
#	sequence/sequence-format.C tree/randomtree.C model.C computation/graph_register.C  probability.C \
#	substitution/substitution-cache.C substitution/substitution-index.C substitution/substitution-star.C tree/tree-util.C \
#	alignment-random.C parameters.C myexception.C monitor.C \
#	tools/tree-dist.C tools/inverse.C distribution.C tools/partition.C \
#	timer_stack.C io.C computation/operator.C computation/expression.C smodel/objects.C \
#	computation/computation.C computation/formula_expression.C smodel/operations.C \
#	distribution-operations.C 2way.C computation/operations.C computation/module.C \
#	computation/loader.C computation/context.C

#---------------------------------------------------------------

trees_distances_SOURCES = tools/trees-distances.C tree/tree.C \
	tree/sequencetree.C tools/tree-dist.C tools/partition.C util.C \
	tree/tree-util.C tools/statistics.C io.C tools/distance-report.C

#---------------------------------------------------------------

draw_tree_SOURCES = tools/draw-tree.C tree/tree.C tree/sequencetree.C \
	tools/tree-dist.C util.C tree/tree-util.C tools/mctree.C rng.C \
	util-random.C tools/partition.C io.C
draw_tree_LDADD = ${CAIRO_LIBS}

#---------------------------------------------------------------

path_graph_SOURCES = tools/path-graph.C alignment/alignment.C sequence/alphabet.C sequence/sequence.C util.C \
	sequence/sequence-format.C alignment/alignment-util.C tree/tree.C io.C tree/sequencetree.C

#---------------------------------------------------------------
# See libtool documentation on plugins:
#    http://www.flameeyes.eu/autotools-mythbuster/libtool/plugins.html

pkglib_LTLIBRARIES = mod.la Prelude.la builtinError.la seq.la join.la reapply.la \
		arraySize.la getIndex.la mkArray.la intToDouble.la negate.la \
		exp.la doubleToLogDouble.la \
		equals.la notequals.la greaterthan.la greaterthanorequal.la lessthan.la \
		lessthanorequal.la getNucleotides.la plus_gwF.la alphabetSize.la \
		iotaUnsigned.la f3x4_frequencies.la gtr.la m0.la set_modifiable_value.la \
		is_modifiable.la is_changeable.la get_modifiable_index.la get_modifiable_for_index.la \
		set_vector_index.la new_vector.la get_vector_index.la vector_size.la \
		c_fst.la c_snd.la c_pair.la new_modifiable.la evaluate.la get_modifiable_value.la \
		show.la rs07_branch_HMM.la rs07_lengthp.la pairwise_alignment_length1.la \
		pairwise_alignment_length2.la transition_counts.la pairwise_alignment_probability_from_counts.la \
		popgen.la Distribution.la Range.la SModel.la trigger.la sum_out_coals.la

mod_la_SOURCES = computation/builtins/mod.C
mod_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

builtinError_la_SOURCES = computation/builtins/builtinError.C
builtinError_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

seq_la_SOURCES = computation/builtins/seq.C
seq_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

join_la_SOURCES = computation/builtins/join.C
join_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

reapply_la_SOURCES = computation/builtins/reapply.C
reapply_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

arraySize_la_SOURCES = computation/builtins/arraySize.C
arraySize_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

getIndex_la_SOURCES = computation/builtins/getIndex.C
getIndex_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

mkArray_la_SOURCES = computation/builtins/mkArray.C
mkArray_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

intToDouble_la_SOURCES = computation/builtins/intToDouble.C
intToDouble_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

doubleToLogDouble_la_SOURCES = computation/builtins/doubleToLogDouble.C
doubleToLogDouble_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

negate_la_SOURCES = computation/builtins/negate.C
negate_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

exp_la_SOURCES = computation/builtins/exp.C
exp_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

Prelude_la_SOURCES = computation/builtins/Prelude.C
Prelude_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

equals_la_SOURCES = computation/builtins/equals.C
equals_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

notequals_la_SOURCES = computation/builtins/notequals.C
notequals_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

greaterthan_la_SOURCES = computation/builtins/greaterthan.C
greaterthan_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

greaterthanorequal_la_SOURCES = computation/builtins/greaterthanorequal.C
greaterthanorequal_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

lessthan_la_SOURCES = computation/builtins/lessthan.C
lessthan_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

lessthanorequal_la_SOURCES = computation/builtins/lessthanorequal.C
lessthanorequal_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

getNucleotides_la_SOURCES = computation/builtins/getNucleotides.C
getNucleotides_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

plus_gwF_la_SOURCES = computation/builtins/plus_gwF.C
plus_gwF_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

alphabetSize_la_SOURCES = computation/builtins/alphabetSize.C
alphabetSize_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

iotaUnsigned_la_SOURCES = computation/builtins/iotaUnsigned.C
iotaUnsigned_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

f3x4_frequencies_la_SOURCES = computation/builtins/f3x4_frequencies.C
f3x4_frequencies_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

gtr_la_SOURCES = computation/builtins/gtr.C
gtr_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

m0_la_SOURCES = computation/builtins/m0.C
m0_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

set_modifiable_value_la_SOURCES = computation/builtins/set_modifiable_value.C
set_modifiable_value_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

is_modifiable_la_SOURCES = computation/builtins/is_modifiable.C
is_modifiable_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

is_changeable_la_SOURCES = computation/builtins/is_changeable.C
is_changeable_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

get_modifiable_index_la_SOURCES = computation/builtins/get_modifiable_index.C
get_modifiable_index_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

get_modifiable_for_index_la_SOURCES = computation/builtins/get_modifiable_for_index.C
get_modifiable_for_index_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

set_vector_index_la_SOURCES = computation/builtins/set_vector_index.C
set_vector_index_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

new_vector_la_SOURCES = computation/builtins/new_vector.C
new_vector_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

get_vector_index_la_SOURCES = computation/builtins/get_vector_index.C
get_vector_index_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

vector_size_la_SOURCES = computation/builtins/vector_size.C
vector_size_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

c_fst_la_SOURCES = computation/builtins/c_fst.C
c_fst_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

c_snd_la_SOURCES = computation/builtins/c_snd.C
c_snd_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

c_pair_la_SOURCES = computation/builtins/c_pair.C
c_pair_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

new_modifiable_la_SOURCES = computation/builtins/new_modifiable.C
new_modifiable_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

evaluate_la_SOURCES = computation/builtins/evaluate.C
evaluate_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

get_modifiable_value_la_SOURCES = computation/builtins/get_modifiable_value.C
get_modifiable_value_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

rs07_branch_HMM_la_SOURCES = computation/builtins/rs07_branch_HMM.C
rs07_branch_HMM_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

rs07_lengthp_la_SOURCES = computation/builtins/rs07_lengthp.C
rs07_lengthp_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

pairwise_alignment_length1_la_SOURCES = computation/builtins/pairwise_alignment_length1.C
pairwise_alignment_length1_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

pairwise_alignment_length2_la_SOURCES = computation/builtins/pairwise_alignment_length2.C
pairwise_alignment_length2_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

transition_counts_la_SOURCES = computation/builtins/transition_counts.C
transition_counts_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

pairwise_alignment_probability_from_counts_la_SOURCES = computation/builtins/pairwise_alignment_probability_from_counts.C
pairwise_alignment_probability_from_counts_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

popgen_la_SOURCES = computation/builtins/popgen.C
popgen_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

trigger_la_SOURCES = computation/builtins/trigger.C
trigger_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

Distribution_la_SOURCES = computation/builtins/Distribution.C
Distribution_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

Range_la_SOURCES = computation/builtins/Range.C
Range_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

SModel_la_SOURCES = computation/builtins/SModel.C
SModel_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

show_la_SOURCES = computation/builtins/show.C
show_la_LDFLAGS = -module -shared -avoid-version -export-dynamic

sum_out_coals_la_SOURCES = computation/builtins/sum_out_coals.C
sum_out_coals_la_LDFLAGS = -module -shared -avoid-version -export-dynamic
