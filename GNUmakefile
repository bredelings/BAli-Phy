# Insertions and deletions work:  "crossovers" and duplications don't.

all: sampler

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
LANGO = fast-math tracer prefetch-loop-arrays omit-frame-pointer#  profile-use
DEBUG = pipe g3 #gdwarf-2 #pg 
EXACTFLAGS =  # --param max-inline-insns-single=1000 --param max-inline-insns-auto=150
DEFS =   NDEBUG NDEBUG_DP #__NO_MATH_INLINES # USE_UBLAS
WARN = all no-sign-compare overloaded-virtual # effc++
OPT =  march=pentium4 O3 # malign-double
LDFLAGS = # -fprofile-generate #-pg # -static
LI=${CXX}

#------------------- Main 
PROGNAME = sampler
NAME = sampler
SOURCES = sequence.C tree.C alignment.C substitution.C moves.C \
          rng.C branch-sample.C exponential.C \
          eigenvalue.C parameters.C likelihood.C mcmc.C \
	  choose.C sequencetree.C branch-lengths.C arguments.C \
	  util.C randomtree.C alphabet.C smodel.C sampler.C \
	  sample-tri.C dpmatrix.C 3way.C 2way.C branch-sample2.C \
	  sample-node.C imodel.C 5way.C sample-topology-NNI.C inverse.C \
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

tools/alignment-blame: alignment.o arguments.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o tools/optimize.o tools/findroot.o tools/alignmentutil.o \
	setup.o smodel.o rates.o exponential.o eigenvalue.o sequence-format.o ${GSLLIBS}

tools/alignment-reorder: alignment.o arguments.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o tools/optimize.o tools/findroot.o setup.o smodel.o \
	rates.o exponential.o eigenvalue.o sequence-format.o randomtree.o ${GSLLIBS}

tools/truckgraph: alignment.o arguments.o alphabet.o sequence.o util.o rng.o ${LIBS:%=-l%}

tools/truckgraph2: alignment.o arguments.o alphabet.o sequence.o util.o \
		tools/alignmentutil.o rng.o ${GSLLIBS}

tools/truckgraph3d: alignment.o arguments.o alphabet.o sequence.o util.o rng.o ${LIBS:%=-l%}

tools/treecount: tree.o sequencetree.o arguments.o util.o rng.o tools/statistics.o ${LIBS:%=-l%}

tools/treedist: tree.o sequencetree.o arguments.o

tools/tree-to-srq: tree.o sequencetree.o arguments.o

tools/srq-to-plot: arguments.o

tools/srqanalyze: arguments.o rng.o tools/statistics.o ${LIBS:%=-l%}

tools/reroot: tree.o sequencetree.o arguments.o

tools/make_random_tree: tree.o sequencetree.o arguments.o util.o\
	 rng.o  ${LIBS:%=-l%}

tools/drawalignment: tree.o alignment.o sequencetree.o arguments.o \
	alphabet.o sequence.o sequence-format.o util.o setup.o rng.o\
	randomtree.o ${LINKLIBS} 

tools/phy_to_fasta: alignment.o sequence.o arguments.o alphabet.o \
	rng.o util.o sequence-format.o ${LIBS:%=-l%}

tools/analyze_distances: alignment.o alphabet.o sequence.o arguments.o\
	util.o sequencetree.o substitution.o eigenvalue.o tree.o sequencetree.o \
	parameters.o exponential.o setup-smodel.o smodel.o imodel.o rng.o likelihood.o \
	dpmatrix.o choose.o tools/optimize.o inverse.o setup.o rates.o matcache.o \
	sequence-format.o randomtree.o ${LINKLIBS}

tools/statreport: tools/statistics.o

tools/findalign: alignment.o alphabet.o arguments.o sequence.o tools/alignmentutil.o \
	rng.o ${GSLLIBS} util.o sequence-format.o

tools/model_P: tools/statistics.o rng.o arguments.o ${LINKLIBS} 

tools/alignment-translate: alignment.o alphabet.o sequence.o arguments.o sequence-format.o \
	util.o

tools/tree-names-trunc: tree.o sequencetree.o arguments.o util.o


#-----------------Other Files
OTHERFILES += 

#------------------- End
DEVEL = ../..
includes += /usr/local/include/
includes += .
src      += 
include $(DEVEL)/GNUmakefile
CC=gcc-3.4
CXX=g++-3.4