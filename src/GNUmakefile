program: bali-phy

TOOLS = tools/model_P tools/statreport tools/alignment-blame \
	tools/alignment-reorder tools/alignment-draw tools/findalign \
	tools/tree-to-srq tools/tree-names-trunc tools/tree-reroot \
	tools/srq-to-plot tools/srq-analyze tools/phy_to_fasta \
	tools/tree-dist-compare tools/cut-range tools/alignment-chop-internal

tools: ${TOOLS}

all: bali-phy tools

install: program tools
	mv bali-phy ${TOOLS} ../bin

#----------------- Definitions
EXACTFLAGS = -pipe 

LANGO := fast-math abi-version=0
WARN  := all no-sign-compare overloaded-virtual strict-aliasing
LDFLAGS := 

ifeq (${DEBUG},y)
  DEBUG := g3
  DEFS += _STLP_DEBUG _GLIBCXX_DEBUG
else
  ifeq ($(origin DEFS), undefined)
     DEFS += NDEBUG_UBLAS NDEBUG_DP NDEBUG
  endif

  ifneq ($(origin ARCH), undefined)
    OPT += march=${ARCH}
  endif	

  OPT += O3
#  LANGO += tracer prefetch-loop-arrays 

  ifneq (${PROF},y)
    LANGO += omit-frame-pointer
  endif
#  EXACTFLAGS += --param inline-unit-growth=300 --param max-inline-insns-auto=300
endif

ifeq (${PROF},y)
  LDFLAGS += -pg
  DEBUG   += pg
endif

ifeq (${STATIC},y)
  .LIBPATTERNS = lib%.a
  LDFLAGS += -static
endif

#------------------- Main 
PROGNAME = bali-phy
SOURCES = sequence.C tree.C alignment.C substitution.C moves.C \
          rng.C exponential.C eigenvalue.C parameters.C likelihood.C mcmc.C \
	  choose.C sequencetree.C sample-branch-lengths.C arguments.C \
	  util.C randomtree.C alphabet.C smodel.C bali-phy.C \
	  sample-tri.C hmm.C dp-engine.C 3way.C 2way.C sample-alignment.C \
	  sample-node.C imodel.C 5way.C sample-topology-NNI.C \
	  setup.C rates.C matcache.C sample-two-nodes.C sequence-format.C \
	  util-random.C alignment-random.C setup-smodel.C sample-topology-SPR.C \
	  alignment-sums.C alignment-util.C probability.C model.C le-double.C \
	  alignment-constraint.C substitution-cache.C substitution-star.C \
	  monitor.C

LIBS = gsl gslcblas m
LINKLIBS = ${LIBS:%=-l%}

${PROGNAME} : ${SOURCES:%.C=%.o} ${LINKLIBS}

tools/model_P: tools/statistics.o rng.o arguments.o ${LINKLIBS} 

tools/statreport: tools/statistics.o

tools/alignment-blame: alignment.o arguments.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o tools/optimize.o tools/findroot.o tools/alignmentutil.o \
	setup.o imodel.o probability.o sequence-format.o model.o \
	alignment-random.o alignment-util.o randomtree.o ${LINKLIBS}

tools/alignment-indices: tree.o alignment.o sequencetree.o arguments.o \
	alphabet.o sequence.o sequence-format.o util.o setup.o rng.o\
	randomtree.o alignment-random.o imodel.o probability.o \
	alignment-util.o model.o ${LINKLIBS}

tools/alignment-reorder: alignment.o arguments.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o tools/optimize.o tools/findroot.o setup.o imodel.o \
	sequence-format.o randomtree.o alignment-util.o probability.o alignment-random.o \
	model.o ${LINKLIBS}

tools/alignment-chop-internal: alignment.o arguments.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o setup.o imodel.o sequence-format.o randomtree.o alignment-util.o probability.o alignment-random.o model.o ${LINKLIBS}

#smodel.o \
#	rates.o exponential.o eigenvalue.o sequence-format.o randomtree.o 

tools/alignment-draw: tree.o alignment.o sequencetree.o arguments.o \
	alphabet.o sequence.o sequence-format.o util.o setup.o rng.o\
	randomtree.o alignment-random.o imodel.o probability.o \
	alignment-util.o model.o ${LINKLIBS} 

tools/alignment-translate: alignment.o alphabet.o sequence.o arguments.o sequence-format.o \
	util.o	

tools/findalign: alignment.o alphabet.o arguments.o sequence.o tools/alignmentutil.o \
	rng.o ${LINKLIBS} util.o sequence-format.o

tools/treecount: tree.o sequencetree.o arguments.o util.o rng.o tools/statistics.o ${LINKLIBS}

tools/tree-dist-compare: tree.o sequencetree.o tools/tree-dist.o arguments.o util.o \
	 rng.o tools/statistics.o ${LINKLIBS}

tools/tree-dist-autocorrelation: tree.o sequencetree.o arguments.o tools/tree-dist.o

tools/tree-dist-cvars: tree.o sequencetree.o arguments.o util.o tools/tree-dist.o

tools/tree-to-srq: tree.o sequencetree.o arguments.o

tools/tree-names-trunc: tree.o sequencetree.o arguments.o util.o

tools/tree-reroot: tree.o sequencetree.o arguments.o

tools/srq-to-plot: arguments.o

tools/srq-analyze: arguments.o rng.o tools/statistics.o ${LINKLIBS}

tools/make_random_tree: tree.o sequencetree.o arguments.o util.o\
	 rng.o  ${LINKLIBS}

tools/phy_to_fasta: alignment.o sequence.o arguments.o alphabet.o \
	rng.o util.o sequence-format.o ${LINKLIBS}

tools/analyze_distances: alignment.o alphabet.o sequence.o arguments.o\
	util.o sequencetree.o substitution.o eigenvalue.o tree.o sequencetree.o \
	parameters.o exponential.o setup-smodel.o smodel.o imodel.o rng.o likelihood.o \
	dpmatrix.o choose.o tools/optimize.o inverse.o setup.o rates.o matcache.o \
	sequence-format.o randomtree.o model.o ${LINKLIBS}

tools/cut-range: arguments.o util.o

tools/truckgraph: alignment.o arguments.o alphabet.o sequence.o util.o rng.o ${LINKLIBS}

tools/truckgraph2: alignment.o arguments.o alphabet.o sequence.o util.o \
		tools/alignmentutil.o rng.o ${LINKLIBS}

tools/truckgraph3d: alignment.o arguments.o alphabet.o sequence.o util.o rng.o ${LINKLIBS}

#------------------- End

SHELL = /bin/sh
MAKEFILES = GNUmakefile

# includes += /home/bredelin/STLport/STLport/stlport
includes += /usr/local/include
includes += ../include/
includes += .

CC=gcc-3.4
CXX=g++-3.4
CPP = $(CC) -E  	# This might vary from machine to machine
CPPFLAGS += $(patsubst %,-I%,$(subst :, ,$(includes)))
CXXFLAGS += ${LANGO:%=-f%} ${WARN:%=-W%} ${DEBUG:%=-%} ${OPT:%=-%} ${DEFS:%=-D%} ${EXACTFLAGS}
LI=${CXX}


GNUmakefile : ${SOURCES:%=.%.d} 

% : %.o
	${LI} ${LDFLAGS} $^ -o $@ ${LOADLIBS}

.%.d : %
	@echo ${shell \
	  ${CPP} -MM ${CPPFLAGS} $< | sed 's/\(^.*\):/$@ \1:/g'} > $@
clean:
	-@rm -f *.o *~ *# *.tar *.gz ${PROGNAME} Makefile core ${TOOLS} tools/*.o

# we are missing sources in the tools/ directory ...
ALLSOURCES=${SOURCES}
-include ${ALLSOURCES:%=.%.d}