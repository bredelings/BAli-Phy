# Insertions and deletions work:  "crossovers" and duplications don't.

all: tree2

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
LANGO = # prefetch-loop-arrays fast-math unroll-loops
DEBUG = pipe g # pg
DEFS = # NDEBUG 
WARN = all no-sign-compare
OPT =  O# 3 malign-double mfpmath=sse msse march=pentium3
LDFLAGS =  # -pg 

#------------------- Main 
PROGNAME = tree2
NAME = tree2
SOURCES = sequence.C etree.C alignment.C substitution.C gaps.C seq_tree.C \
          moves.C myrandom.C possibilities.C sample.C sample2.C exponential.C \
          eigenvalue.C
LIBS = 
PROGNAMES = ${NAME} rna
ALLSOURCES = ${SOURCES} 

${NAME} : ${SOURCES:%.C=%.o} ${LIBS:%=-l%}

rna : ${SOURCES:%.C=%.o} ${LIBS:%=-l%}

#-----------------Other Files
OTHERFILES += 

#------------------- End
DEVEL = ..
includes += 
src      += 
include $(DEVEL)/GNUmakefile
CC=gcc-3.2
CXX=g++-3.2

