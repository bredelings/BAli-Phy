# Insertions and deletions work:  "crossovers" and duplications don't.

all: bali-phy

# Hurts:
# -mfpmath=sse     1:23 -> 1:30
# -mfpmath=387,sse 1:22->1:24

# No effect:
# -malign-double
# -mmmx, -msse, -msse2 (w/ or w/o -mfpmath=sse)
# -O3

# Helps:
# -fomit-frame-pointer: 1:23.7 -> 1:23.2
# -ffast-math 
# -fprefetch-loop-arrays
# -march=pentium4

# -fexpensive-optimizations
# -fno-strength-reduce?
# -fno-exceptions -fno-rtti


# try -ffast-math, -march=pentium4, -malign-double, -mfpmath=sse,387
#     -msse2

# -fomit-frame-pointer -pipe -fexpensive-optimizations -fpic -frerun-cse-after-loop -frerun-loop-opt -foptimize-register-move
# -freorder-blocks

#-mfpmath=sse,387 ?

#----------------- Definitions
LANGO = fast-math  tracer prefetch-loop-arrays omit-frame-pointer # profile-use
DEBUG = pipe # g3 #gdwarf-2 #pg 
EXACTFLAGS =  # --param max-inline-insns-single=1000 --param max-inline-insns-auto=150
DEFS =   NDEBUG_UBLAS # NDEBUG_DP # NDEBUG #__NO_MATH_INLINES # USE_UBLAS
WARN = all no-sign-compare overloaded-virtual strict-aliasing # effc++
OPT =  march=pentium4 O3 # malign-double
LDFLAGS = # -fprofile-generate #-pg # -static
LI=${CXX}

#------------------- Main 
PROGNAME = bali-phy
NAME = bali-phy
SOURCES = sequence.C tree.C alignment.C substitution.C moves.C \
          rng.C exponential.C eigenvalue.C parameters.C likelihood.C mcmc.C \
	  choose.C sequencetree.C sample-branch-lengths.C arguments.C \
	  util.C randomtree.C alphabet.C smodel.C bali-phy.C \
	  sample-tri.C hmm.C dp-engine.C 3way.C 2way.C sample-alignment.C \
	  sample-node.C imodel.C 5way.C sample-topology-NNI.C \
	  setup.C rates.C matcache.C sample-two-nodes.C sequence-format.C \
	  util-random.C alignment-random.C setup-smodel.C sample-topology-SPR.C \
	  alignment-sums.C

LIBS = gsl gslcblas m 
GSLLIBS = ${LIBS:%=-l%}
SLIBS =  #lapack cblas atlas # gsl gslcblas m 
LINKLIBS = ${LIBS:%=-l%} ${SLIBS:%=lib%.a} /usr/local/lib/liblapack.a /usr/local/lib/libcblas.a /usr/local/lib/libatlas.a
PROGNAMES = ${NAME} 
ALLSOURCES = ${SOURCES} 

${NAME} : ${SOURCES:%.C=%.o} ${LINKLIBS}

tools/model_P: tools/statistics.o rng.o arguments.o ${LINKLIBS} 

tools/statreport: tools/statistics.o

tools/alignment-blame: alignment.o arguments.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o tools/optimize.o tools/findroot.o tools/alignmentutil.o \
	setup.o smodel.o rates.o exponential.o eigenvalue.o sequence-format.o \
	alignment-random.o randomtree.o ${GSLLIBS}

tools/alignment-reorder: alignment.o arguments.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o tools/optimize.o tools/findroot.o setup.o smodel.o \
	rates.o exponential.o eigenvalue.o sequence-format.o randomtree.o ${GSLLIBS}

tools/alignment-draw: tree.o alignment.o sequencetree.o arguments.o \
	alphabet.o sequence.o sequence-format.o util.o setup.o rng.o\
	randomtree.o alignment-random.o ${LINKLIBS} 

tools/alignment-translate: alignment.o alphabet.o sequence.o arguments.o sequence-format.o \
	util.o	

tools/findalign: alignment.o alphabet.o arguments.o sequence.o tools/alignmentutil.o \
	rng.o ${GSLLIBS} util.o sequence-format.o

tools/treecount: tree.o sequencetree.o arguments.o util.o rng.o tools/statistics.o ${LIBS:%=-l%}

tools/tree-dist-compare: tree.o sequencetree.o tools/tree-dist.o arguments.o util.o rng.o tools/statistics.o ${LIBS:%=-l%}

tools/tree-dist-autocorrelation: tree.o sequencetree.o sequencetree.o arguments.o tools/tree-dist.o

tools/tree-to-srq: tree.o sequencetree.o arguments.o

tools/tree-names-trunc: tree.o sequencetree.o arguments.o util.o

tools/tree-reroot: tree.o sequencetree.o arguments.o

tools/srq-to-plot: arguments.o

tools/srq-analyze: arguments.o rng.o tools/statistics.o ${LIBS:%=-l%}

tools/make_random_tree: tree.o sequencetree.o arguments.o util.o\
	 rng.o  ${LIBS:%=-l%}

tools/phy_to_fasta: alignment.o sequence.o arguments.o alphabet.o \
	rng.o util.o sequence-format.o ${LIBS:%=-l%}

tools/analyze_distances: alignment.o alphabet.o sequence.o arguments.o\
	util.o sequencetree.o substitution.o eigenvalue.o tree.o sequencetree.o \
	parameters.o exponential.o setup-smodel.o smodel.o imodel.o rng.o likelihood.o \
	dpmatrix.o choose.o tools/optimize.o inverse.o setup.o rates.o matcache.o \
	sequence-format.o randomtree.o ${LINKLIBS}

tools/truckgraph: alignment.o arguments.o alphabet.o sequence.o util.o rng.o ${LIBS:%=-l%}

tools/truckgraph2: alignment.o arguments.o alphabet.o sequence.o util.o \
		tools/alignmentutil.o rng.o ${GSLLIBS}

tools/truckgraph3d: alignment.o arguments.o alphabet.o sequence.o util.o rng.o ${LIBS:%=-l%}

#-----------------Other Files
OTHERFILES += 

#------------------- End
DEVEL = ../..
includes += ./include/
includes += .
src      += 
include $(DEVEL)/GNUmakefile
CC=gcc-3.4
CXX=g++-3.4