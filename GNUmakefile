# Insertions and deletions work:  "crossovers" and duplications don't.

all: tree2

# try -ffast-math, -march=pentium4, -malign-double, -mfpmath=sse,387
#     -msse2

#----------------- Definitions
LANGO = # no-inline # fast-math  
DEBUG = g  pg 
DEFS = #NDEBUG 
WARN = all no-sign-compare
OPT =  # O3 march=pentium4 msse2 mfpmath=sse
LDFLAGS = -pg 

#------------------- Main 
PROGNAME = tree2
NAME = tree2
SOURCES = sequence.C etree.C alignment.C substitution.C gaps.C seq_tree.C \
          moves.C myrandom.C possibilities.C sample.C sample2.C
LIBS = 
PROGNAMES = ${NAME}
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

