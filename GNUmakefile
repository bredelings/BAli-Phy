# Insertions and deletions work:  "crossovers" and duplications don't.

all: sampler

# -fomit-frame-pointer
# -fprefetch-loop-arrays
# -ffast-math 

# -fexpensive-optimizations
# -fno-strength-reduce?
# -fno-exceptions -fno-rtti

# try -ffast-math, -march=pentium4, -malign-double, -mfpmath=sse,387
#     -msse2

# -mmmx -m3dnow -msse
# -mmmx -msse   -msse2
# -fomit-frame-pointer -pipe -fexpensive-optimizations -fpic -frerun-cse-after-loop -frerun-loop-opt -foptimize-register-move
# -freorder-blocks -fprefetch-loop-arrays

#-mfpmath=sse,387 ?

#----------------- Definitions
LANGO = fast-math unroll-loops prefetch-loop-arrays abi-version=0
DEBUG = pipe g3 pg
EXACTFLAGS = --param max-inline-insns-single=1000 --param max-inline-insns-auto=150
DEFS = NDEBUG NDEBUG_DP 
WARN = all no-sign-compare overloaded-virtual
OPT =  malign-double mfpmath=sse msse mmmx msse2 march=pentium4 O3
LDFLAGS = -pg # -static 
LI=${CXX}

#------------------- Main 
PROGNAME = sampler
NAME = sampler
SOURCES = sequence.C tree.C alignment.C substitution.C moves.C \
          rng.C branch-sample.C exponential.C \
          eigenvalue.C parameters.C likelihood.C mcmc.C topology-sample.C \
	  choose.C sequencetree.C branch-lengths.C arguments.C \
	  util.C randomtree.C alphabet.C smodel.C sampler.C \
	  tri-sample.C dpmatrix.C 3way.C 2way.C branch-sample2.C \
	  node-sample2.C imodel.C 5way.C topology-sample2.C inverse.C \
	  setup.o

LIBS = gsl gslcblas m 
PROGNAMES = ${NAME} 
ALLSOURCES = ${SOURCES} 

${NAME} : ${SOURCES:%.C=%.o} ${LIBS:%=-l%} /usr/local/lib/liblapack.a /usr/local/lib/libcblas.a /usr/local/lib/libatlas.a # libgsl.a libgslcblas.a libm.a


bin/alignment-blame: alignment.o arguments.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o bin/optimize.o bin/findroot.o \
	setup.o exponential.o eigenvalue.o \  # needed for setup
	${LIBS:%=-l%}

bin/truckgraph: alignment.o arguments.o alphabet.o sequence.o util.o rng.o ${LIBS:%=-l%}

bin/truckgraph2: alignment.o arguments.o alphabet.o sequence.o util.o rng.o ${LIBS:%=-l%}

bin/truckgraph3d: alignment.o arguments.o alphabet.o sequence.o util.o rng.o ${LIBS:%=-l%}

bin/treecount: tree.o sequencetree.o arguments.o util.o rng.o ${LIBS:%=-l%}

bin/treedist: tree.o sequencetree.o arguments.o

bin/treetosrq: tree.o sequencetree.o arguments.o

bin/srqtoplot: arguments.o

bin/srqanalyze: arguments.o rng.o ${LIBS:%=-l%}

bin/reroot: tree.o sequencetree.o arguments.o

bin/make_random_tree: tree.o sequencetree.o arguments.o util.o\
	 rng.o  ${LIBS:%=-l%}

bin/drawalignment: tree.o alignment.o sequencetree.o arguments.o \
	rng.o alphabet.o sequence.o util.o setup.o smodel.o exponential.o \
	eigenvalue.o ${LIBS:%=-l%}

bin/phy_to_fasta: alignment.o sequence.o arguments.o alphabet.o \
	rng.o util.o ${LIBS:%=-l%}

bin/analyze_distances: alignment.o alphabet.o sequence.o arguments.o alphabet.o \
	util.o sequencetree.o substitution.o eigenvalue.o tree.o sequencetree.o \
	parameters.o exponential.o smodel.o imodel.o rng.o likelihood.o \
	dpmatrix.o choose.o bin/optimize.o inverse.o setup.o ${LIBS:%=-l%} /usr/local/lib/liblapack.a /usr/local/lib/libcblas.a /usr/local/lib/libatlas.a

#-----------------Other Files
OTHERFILES += 

#------------------- End
DEVEL = ../..
includes += /usr/local/include/
includes += .
src      += 
include $(DEVEL)/GNUmakefile
# CC=gcc-3.4
# CXX=g++-3.4