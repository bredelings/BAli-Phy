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


# try -fforce-addr

#----------------- Definitions
LANGO = prefetch-loop-arrays fast-math unroll-loops
DEBUG = pipe g # pg
DEFS = #NDEBUG 
WARN = all no-sign-compare
OPT =  malign-double mfpmath=sse msse2 march=pentium4
LDFLAGS =  # -pg 

#------------------- Main 
PROGNAME = sampler
NAME = sampler
SOURCES = sequence.C tree.C alignment.C substitution.C gaps.C moves.C \
          rng.C possibilities.C node-sample.C branch-sample.C exponential.C \
          eigenvalue.C parameters.C likelihood.C mcmc.C
LIBS = gsl gslcblas m
PROGNAMES = ${NAME} 
ALLSOURCES = ${SOURCES} 

${NAME} : ${SOURCES:%.C=%.o} ${LIBS:%=-l%}

#-----------------Other Files
OTHERFILES += 

#------------------- End
DEVEL = ..
includes += 
src      += 
include $(DEVEL)/GNUmakefile
# CC=gcc-3.2
# CXX=g++-3.2

