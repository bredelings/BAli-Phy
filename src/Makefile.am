#bin_SCRIPTS = checkjobs sge_kill getnames.pl bali-phy.sh GNUmakefile  genreport.pl pickout.pl run.sh

CXXFLAGS = ${CHIP}

if GCC

CXXFLAGS += -ffast-math -pipe -pedantic

INCLUDES = -I@top_srcdir@/include

.LIBPATTERNS = 

if HOST_LINUX
if STATIC
LDFLAGS += -static
.LIBPATTERNS += lib%.a
else
.LIBPATTERNS += lib%.so lib%.a
endif
endif

if HOST_DARWIN
CXXFLAGS += -fvisibility=default
INCLUDES += /opt/sw/include
vpath %.dylib /opt/sw/lib
vpath %.a /opt/sw/lib
if STATIC
.LIBPATTERNS += lib%.a lib%.dylib 
else
.LIBPATTERNS += lib%.dylib lib%.a 
endif
endif

if HOST_MSWINDOWS
if STATIC
LDFLAGS += -static
.LIBPATTERNS += lib%.a
else
.LIBPATTERNS += lib%.dll lib%.a
endif
endif

if !DEBUG
CXXFLAGS += -O3 -funroll-loops -fweb -fomit-frame-pointer
# we can't choose between nocona & k8 only based on HOST_X86_64
endif

endif

if DEBUG
DBG = debug
CXXFLAGS += -g
else
DBG =
CXXFLAGS += -DNDEBUG
endif
noinst_PROGRAMS = $(DBG)

CXXFLAGS += -Wall -Wextra -Wno-sign-compare -Woverloaded-virtual -Wstrict-aliasing

BOOST_LIBS = -lboost_program_options${BOOST_SUFFIX}

LIBS = -lgsl -lgslcblas -lm ${BOOST_LIBS}

LDADD = ${LIBS}

#-----------------------------------------------------------------

TOOLS = model_P statreport alignment-gild \
	alignment-reorder alignment-draw alignment-find \
	trees-to-SRQ tree-reroot alignment-chop-internal \
	alignment-convert alignment-thin \
	trees-consensus cut-range alignment-translate \
	alignment-median alignment-indices pickout \
	joint-indels alignment-consensus alignment-info \
	subsample tree-mean-lengths trees-bootstrap \
	joint-parsimony

bin_PROGRAMS = bali-phy ${TOOLS}

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
	  tools/tree-dist.C

#-----------------------------------------------------------------

model_P_SOURCES = tools/model_P.C tools/statistics.C rng.C

model_P_LDADD = -lm ${BOOST_LIBS} 

#-------------------------- statreport --------------------------

statreport_SOURCES = tools/statreport.C tools/statistics.C

statreport_LDADD = ${BOOST_LIBS}

#---------------------------------------------------------------

alignment_gild_SOURCES = tools/alignment-gild.C alignment.C alphabet.C \
	sequence.C util.C rng.C tree.C sequencetree.C tools/optimize.C \
	tools/findroot.C setup.C imodel.C probability.C sequence-format.C \
	model.C tools/distance-methods.C alignment-random.C alignment-util.C \
	randomtree.C tree-util.C tools/inverse.C

alignment_gild_LDADD = ${LIBS} ${ATLAS_LIBS}

#---------------------------------------------------------------

alignment_median_SOURCES = tools/alignment-median.C alignment.C alphabet.C sequence.C util.C rng.C \
	tree.C sequencetree.C util-random.C \
	sequence-format.C alignment-util.C 

#---------------------------------------------------------------

alignment_consensus_SOURCES = tools/alignment-consensus.C alignment.C alphabet.C sequence.C util.C rng.C \
	tree.C sequencetree.C util-random.C \
	sequence-format.C alignment-util.C 

alignment_consensus_LDADD = ${LIBS}

#---------------------------------------------------------------

alignment_reorder_SOURCES = tools/alignment-reorder.C alignment.C alphabet.C sequence.C util.C rng.C \
	tree.C sequencetree.C tools/optimize.C tools/findroot.C setup.C imodel.C \
	sequence-format.C randomtree.C alignment-util.C probability.C alignment-random.C \
	model.C tree-util.C 

#---------------------------------------------------------------

alignment_thin_SOURCES = tools/alignment-thin.C alignment.C alphabet.C sequence.C util.C rng.C \
	tree.C sequencetree.C setup.C imodel.C sequence-format.C randomtree.C \
	alignment-util.C probability.C alignment-random.C model.C tree-util.C

#---------------------------------------------------------------

alignment_chop_internal_SOURCES = tools/alignment-chop-internal.C alignment.C alphabet.C sequence.C util.C tree.C \
	sequence-format.C alignment-util.C 

alignment_chop_internal_LDADD = ${LIBS}

#---------------------------------------------------------------

alignment_indices_SOURCES = tools/alignment-indices.C alignment.C alphabet.C sequence.C util.C tree.C sequence-format.C alignment-util.C 

#---------------------------------------------------------------

alignment_draw_SOURCES = tools/alignment-draw.C alignment.C alphabet.C sequence.C sequence-format.C util.C alignment-util.C tools/colors.C tree.C   

#---------------------------------------------------------------

joint_indels_SOURCES = tools/joint-indels.C alignment.C alphabet.C sequence.C util.C rng.C tree.C sequencetree.C tree-util.C setup.C imodel.C probability.C sequence-format.C model.C alignment-random.C alignment-util.C randomtree.C tools/tree-dist.C tools/statistics.C tools/joint-A-T.C 

#---------------------------------------------------------------

joint_parsimony_SOURCES = tools/joint-parsimony.C alignment.C alphabet.C sequence.C util.C rng.C tree.C \
	sequencetree.C tree-util.C setup.C imodel.C probability.C sequence-format.C \
	model.C alignment-random.C alignment-util.C randomtree.C \
	tools/parsimony.C tools/joint-A-T.C  

#---------------------------------------------------------------

alignment_info_SOURCES = tools/alignment-info.C alignment.C alphabet.C sequence.C util.C rng.C tree.C sequencetree.C setup.C imodel.C tools/parsimony.C sequence-format.C randomtree.C alignment-util.C probability.C alignment-random.C model.C tree-util.C 

#---------------------------------------------------------------

alignment_translate_SOURCES = tools/alignment-translate.C alignment.C alignment-util.C alphabet.C sequence.C sequence-format.C util.C tree.C 

alignment_translate_LDADD = ${BOOST_LIBS}

#---------------------------------------------------------------

alignment_find_SOURCES = tools/alignment-find.C alignment.C alphabet.C sequence.C alignment-util.C rng.C util.C sequence-format.C tree.C 

#---------------------------------------------------------------

alignment_convert_SOURCES = tools/alignment-convert.C alignment.C alignment-util.C sequence.C alphabet.C util.C sequence-format.C tree.C 

#---------------------------------------------------------------

trees_consensus_SOURCES = tools/trees-consensus.C tree.C sequencetree.C tools/tree-dist.C util.C tools/statistics.C 

#---------------------------------------------------------------

trees_bootstrap_SOURCES = tools/trees-bootstrap.C tree.C sequencetree.C tools/tree-dist.C util.C rng.C tools/statistics.C tools/bootstrap.C 

#---------------------------------------------------------------
tree_mean_lengths_SOURCES = tools/tree-mean-lengths.C util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C 

tree_mean_lengths_LDADD = ${BOOST_LIBS}

#---------------------------------------------------------------

trees_to_SRQ_SOURCES = tools/trees-to-SRQ.C util.C tree.C sequencetree.C tools/tree-dist.C tools/statistics.C tree-util.C

trees_to_SRQ_LDADD = ${BOOST_LIBS} 

#---------------------------------------------------------------

tree_reroot_SOURCES = tools/tree-reroot.C tree.C sequencetree.C tree-util.C util.C

tree_reroot_LDADD = ${BOOST_LIBS} 

#---------------------------------------------------------------

pickout_SOURCES = tools/pickout.C

#---------------------------------------------------------------

subsample_SOURCES = tools/subsample.C

#---------------------------------------------------------------

cut_range_SOURCES = tools/cut-range.C util.C

cut_range_LD_ADD = ${BOOST_LIBS}

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

analyze_distances: alignment.o alphabet.o sequence.o tools/distance-methods.o \
	util.o sequencetree.o substitution.o eigenvalue.o tree.o sequencetree.o \
	exponential.o setup-smodel.o smodel.o imodel.o rng.o likelihood.o \
	choose.o tools/optimize.o setup.o rates.o matcache.o alignment-util.o \
	sequence-format.o randomtree.o model.o  probability.o \
	substitution-cache.o substitution-index.o substitution-star.o tree-util.o \
	alignment-random.o parameters.o myexception.o monitor.o ${LIBS}

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

truckgraph2: alignment.o alphabet.o sequence.o util.o \
		alignment-util.o rng.o ${LIBS}

#---------------------------------------------------------------

truckgraph3d: alignment.o alphabet.o sequence.o util.o rng.o 
