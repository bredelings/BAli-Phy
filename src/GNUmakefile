# Mac: search for libraries in the fink path
.LIBPATTERNS = lib%.dylib lib%.so lib%.a
vpath %.dylib /sw/lib
vpath %.so /sw/lib
vpath %.a /sw/lib

program: bali-phy

TOOLS = tools/model_P tools/statreport tools/alignment-gild \
	tools/alignment-reorder tools/alignment-draw tools/alignment-find \
	tools/tree-to-srq tools/tree-reroot tools/alignment-chop-internal \
	tools/srq-to-plot tools/srq-analyze tools/alignment-convert \
	tools/tree-dist-compare tools/cut-range tools/alignment-translate \
	tools/alignment-median tools/alignment-indices tools/pickout \
	tools/joint-A-T tools/alignment-consensus tools/alignment-info


tools: ${TOOLS}

all: bali-phy tools

install: program tools
	mkdir -p ../build
	mkdir -p ../build/bin
	cp ${TOOLS} ../build/bin
	cp ../bin/* ../build/bin
	cp bali-phy ../build/bin

#----------------- Definitions
EXACTFLAGS = -pipe

LANGO := fast-math 

WARN  := all no-sign-compare overloaded-virtual strict-aliasing

ifeq (${BOUNDS_CHECK},y)
  DEFS += _GLIBCXX_DEBUG
endif

ifeq (${DEBUG},y)
  DEBUG := g3
else
  ifeq ($(origin DEFS), undefined)
     DEFS += NDEBUG_DP NDEBUG
  endif

  ifneq ($(origin ARCH), undefined)
    OPT += march=${ARCH} mtune=${ARCH}
  endif	

  OPT += O3 # mfpmath=sse,387 msse2
  LANGO += unroll-loops web

  ifneq (${PROF},y)
    LANGO += omit-frame-pointer
  endif

#  EXACTFLAGS += --param max-inline-insns-single=300
#  EXACTFLAGS += --param max-inline-insns-auto=175
#  EXACTFLAGS += --param max-inline-insns-recursive=300
#  EXACTFLAGS += --param max-inline-insns-recursive-auto=300

#  EXACTFLAGS += --param large-function-insns=5700
#  EXACTFLAGS += --param large-function-growth=100
#  EXACTFLAGS += --param inline-unit-growth=300

  EXACTFLAGS += --param max-gcse-passes=2
endif

ifeq (${PROF},y)
  LDFLAGS += -pg
  DEBUG   += pg
endif

ifeq (${STATIC},y)
  .LIBPATTERNS = lib%.a
  LDFLAGS += -static -pthread
else
  LDFLAGS += -rdynamic
endif

#------------------- Main 
PROGNAME = bali-phy
SOURCES = sequence.C tree.C alignment.C substitution.C moves.C \
          rng.C exponential.C eigenvalue.C parameters.C likelihood.C mcmc.C \
	  choose.C sequencetree.C sample-branch-lengths.C \
	  util.C randomtree.C alphabet.C smodel.C bali-phy.C \
	  hmm.C dp-engine.C dp-array.C dp-matrix.C 3way.C 2way.C sample-alignment.C \
	  sample-tri.C sample-node.C imodel.C 5way.C sample-topology-NNI.C \
	  setup.C rates.C matcache.C sample-two-nodes.C sequence-format.C \
	  util-random.C alignment-random.C setup-smodel.C sample-topology-SPR.C \
	  alignment-sums.C alignment-util.C probability.C model.C \
	  alignment-constraint.C substitution-cache.C substitution-star.C \
	  monitor.C substitution-index.C tree-util.C myexception.C pow2.C

LIBS = gsl gslcblas m boost_program_options
ifeq (${STLPORT},y) 
  DEFS += _STLP_DEBUG
  LIBS += stlportstlg
  EXACTFLAGS += -pthread

  VPATH += /usr/local/STLport/lib/
  includes += /usr/include/c++/stlport
endif

LINKLIBS = ${LIBS:%=-l%}

${PROGNAME} : ${SOURCES:%.C=%.o} ${LINKLIBS}

tools/model_P: tools/statistics.o rng.o ${LINKLIBS} 

tools/statreport: tools/statistics.o

tools/alignment-gild: alignment.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o tools/optimize.o tools/findroot.o \
	setup.o imodel.o probability.o sequence-format.o model.o tools/distance-methods.o \
	alignment-random.o alignment-util.o randomtree.o tree-util.o ${LINKLIBS}

tools/alignment-median: alignment.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o util-random.o \
	sequence-format.o alignment-util.o ${LINKLIBS}

tools/alignment-consensus: alignment.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o util-random.o \
	sequence-format.o alignment-util.o ${LINKLIBS}

tools/alignment-reorder: alignment.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o tools/optimize.o tools/findroot.o setup.o imodel.o \
	sequence-format.o randomtree.o alignment-util.o probability.o alignment-random.o \
	model.o tree-util.o ${LINKLIBS}

tools/alignment-thin: alignment.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o setup.o imodel.o sequence-format.o randomtree.o \
	alignment-util.o probability.o alignment-random.o model.o tree-util.o \
	${LINKLIBS}

tools/alignment-chop-internal: alignment.o alphabet.o sequence.o util.o tree.o \
	sequence-format.o alignment-util.o ${LINKLIBS}

tools/alignment-indices: alignment.o alphabet.o sequence.o util.o tree.o \
	sequence-format.o alignment-util.o ${LINKLIBS}

tools/alignment-draw: alignment.o alphabet.o sequence.o sequence-format.o util.o \
	alignment-util.o tools/colors.o tree.o  ${LINKLIBS} 

tools/joint-A-T: alignment.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o tools/optimize.o tools/findroot.o tree-util.o \
	setup.o imodel.o probability.o sequence-format.o model.o tools/distance-methods.o \
	alignment-random.o alignment-util.o randomtree.o tools/tree-dist.o \
	tools/statistics.o ${LINKLIBS}

tools/alignment-info: alignment.o alphabet.o sequence.o util.o rng.o \
	tree.o sequencetree.o tools/optimize.o tools/findroot.o setup.o imodel.o \
	sequence-format.o randomtree.o alignment-util.o probability.o alignment-random.o \
	model.o tree-util.o ${LINKLIBS}

tools/alignment-translate: alignment.o alignment-util.o alphabet.o sequence.o \
	sequence-format.o util.o tree.o -lboost_program_options

tools/alignment-find: alignment.o alphabet.o sequence.o alignment-util.o \
	rng.o util.o sequence-format.o tree.o ${LINKLIBS}

tools/alignment-convert: alignment.o alignment-util.o sequence.o alphabet.o util.o \
	sequence-format.o tree.o -lboost_program_options

tools/tree-dist-compare: tree.o sequencetree.o tools/tree-dist.o util.o \
	 rng.o tools/statistics.o ${LINKLIBS}

tools/tree-dist-autocorrelation: tree.o sequencetree.o tools/tree-dist.o

tools/tree-dist-cvars: tree.o sequencetree.o util.o tools/tree-dist.o

tools/tree-to-srq: util.o tree.o sequencetree.o tools/tree-dist.o tree-util.o -lboost_program_options

tools/tree-reroot: tree.o sequencetree.o tree-util.o -lboost_program_options util.o

tools/srq-to-plot: -lboost_program_options

tools/srq-analyze: rng.o tools/statistics.o ${LINKLIBS}

tools/make_random_tree: tree.o sequencetree.o util.o rng.o  ${LINKLIBS}

tools/analyze_distances: alignment.o alphabet.o sequence.o tools/distance-methods.o \
	util.o sequencetree.o substitution.o eigenvalue.o tree.o sequencetree.o \
	exponential.o setup-smodel.o smodel.o imodel.o rng.o likelihood.o \
	choose.o tools/optimize.o setup.o rates.o matcache.o alignment-util.o \
	sequence-format.o randomtree.o model.o le-double.o ${LINKLIBS} probability.o \
	substitution-cache.o substitution-index.o substitution-star.o tree-util.o \
	alignment-random.o parameters.o 

test-smodel: alignment.o alphabet.o sequence.o tree.o sequencetree.o util.o \
	setup-smodel.o smodel.o randomtree.o model.o sequence-format.o rates.o \
	probability.o rng.o setup-smodel.o exponential.o eigenvalue.o alignment-util.o \
	setup.o	imodel.o alignment-random.o ${LINKLIBS}

tools/cut-range: util.o -lboost_program_options

tools/truckgraph: alignment.o alphabet.o sequence.o util.o rng.o ${LINKLIBS}

tools/truckgraph2: alignment.o alphabet.o sequence.o util.o \
		alignment-util.o rng.o ${LINKLIBS}

tools/truckgraph3d: alignment.o alphabet.o sequence.o util.o rng.o ${LINKLIBS}

tools/hat: rng.o ${LINKLIBS}
#------------------- End

SHELL = /bin/sh
MAKEFILES = GNUmakefile

includes += /usr/local/include
includes += /sw/include
includes += ../include/
includes += .

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
