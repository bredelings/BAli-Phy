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

# try -fforce-addr

#----------------- Definitions
LANGO = fast-math unroll-loops prefetch-loop-arrays abi-version=0 inline-limit=3000 keep-inline-functions
DEBUG = pipe pg # g3
DEFS = NDEBUG 
WARN = all no-sign-compare overloaded-virtual
OPT =  malign-double mfpmath=sse msse mmmx msse2 march=pentium4 O3
LDFLAGS = -pg # -static 
LI=${CXX}

#------------------- Main 
PROGNAME = sampler
NAME = sampler
SOURCES = sequence.C tree.C alignment.C substitution.C moves.C \
          rng.C node-sample.C branch-sample.C exponential.C \
          eigenvalue.C parameters.C likelihood.C mcmc.C topology-sample.C \
	  choose.C sequencetree.C branch-lengths.C arguments.C \
	  util.C randomtree.C alphabet.C smodel.C sampler.C \
	  tri-sample.C dpmatrix.C 3way.C 2way.C branch-sample2.C \
	  node-sample2.C # map.C 

LIBS = gsl gslcblas m
PROGNAMES = ${NAME} 
ALLSOURCES = ${SOURCES} 

${NAME} : ${SOURCES:%.C=%.o} ${LIBS:%=-l%} # libgsl.a libgslcblas.a libm.a


treecount: treecount.o tree.o sequencetree.o arguments.o util.o

reroot: reroot.o tree.o sequencetree.o arguments.o

make_random_tree: randomtree.o tree.o sequencetree.o arguments.o util.o\
	 rng.o -lgsl -lgslcblas -lm

#-----------------Other Files
OTHERFILES += 

#------------------- End
DEVEL = ../..
# includes += /usr/local/include/
src      += 
include $(DEVEL)/GNUmakefile
CC=gcc-3.4
CXX=g++-3.4