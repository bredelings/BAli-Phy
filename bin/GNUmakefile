#!/usr/bin/make -f
SHELL=/bin/sh

TOPOLOGIES=c50 c66 c80 c90 c95 c99 c100

IMODEL=$(shell head -n 40 1.out | pickout --no-header 'indel model')

TREES=${TOPOLOGIES}

ifeq ($(origin CHAIN), undefined)
  CHAIN=1
endif

all: Results/analysis

#.DELETE_ON_ERROR: 

.PRECIOUS .SECONDARY: Results/c%.topology
.PRECIOUS .SECONDARY: Results/%
.PRECIOUS .SECONDARY: Work/%
.PRECIOUS .SECONDARY: Work/%.from.${SKIP}
.PRECIOUS .SECONDARY: Work/%.from.${SKIP}+${SIZE}
.PRECIOUS .SECONDARY: % 

.PRECIOUS .SECONDARY: Results/%-AU.prob

Work/setup:
	@echo -e "Setting up directories... \c"
	@mkdir -p Work/
	@mkdir -p Mixing/
	@mkdir -p Results/
	@touch Work/setup
	@echo "done."


ifeq ($(IMODEL),none)
AU_TARGETS=
else
AU_TARGETS= Results/MAP-AU.html Results/MAP-AU2.html Results/au-graph.plot Results/consensus.fasta Results/consensus-AU.html Results/consensus-AU2.html
endif

Results/analysis: Results/consensus ${TOPOLOGIES:%=Results/%.topology} ${TREES:%=Results/%.tree} \
                  ${TREES:%=Results/%.mtree} Results/MAP.topology Results/MAP.tree \
                  Results/c-levels.plot Results/c-sub-levels.plot Work/trees.from.${SKIP}+${SIZE} \
		  Results/Report Results/MAP.fasta Results/MAP.phy \
		  Mixing/partitions.SRQ Mixing/c50.SRQ Mixing/partitions.bs \
		  $(AU_TARGETS)
	@echo "Analyzed:  $$(wc -l < Work/trees.from.${SKIP}+${SIZE}) trees from iteration $$(( ${SKIP} + 1))." > Results/analysis
	@echo "Finished:  $$(date)" >> Results/analysis
	@echo 
	@cat Results/analysis
	@echo 

Results/Report:  Work/header Work/p Results/MAP.topology
	@{ cat Work/header ; \
	statreport 2: --skip ${SKIP} ${SIZE:%=--max %} < Work/p 2>/dev/null ; \
	cat Results/MAP.topology ; \
	} > $@

Work/header: Work/out Work/Pmarg
	@{ genreport.pl Work/out; echo ; } > $@

Work/samples-${SKIP}+${SIZE}: Work/out
	@cut-range --skip ${SKIP} ${SIZE:%=--size %} < $< > $@

%.from.${SKIP}: %
	@tail -n +$$(( ${SKIP}+2 )) $< > $@

Work/%: ${CHAIN}.% Work/setup
	@cp ${CHAIN}.$* Work/$*

Work/%.from.${SKIP}+${SIZE}: Work/%.from.${SKIP}
	@head -n ${SIZE} $< > $@ 

Work/%.from.${SKIP}+: Work/%.from.${SKIP}
	@cp $< $@ 

Results/consensus : Work/trees Work/trees.from.${SKIP}+${SIZE}
	@echo -e "Summarizing topology distribution... \c";
	@trees-consensus --skip ${SKIP} ${SIZE:%=--max %}  ${MIN:%=--min-support %} $< --consensus 0.5,0.66,0.8,0.9,0.95,0.99,1.0 --sub-partitions > $@
	@echo "done."

Work/partitions: Results/consensus
	@pickout --no-header --large pi < $< > $@

Work/partitions.pred: Work/partitions
	@sed "s/$$/\n/" < $< > $@

Mixing/partitions.bs: Work/partitions.pred Work/trees
	@echo -e "Computing confidence intervals for partition support... \c";
	@trees-bootstrap --skip ${SKIP} ${SIZE:%=--max %} Work/trees --pred Work/partitions.pred > $@
	@echo "done."

Results/c-levels.plot : Results/consensus
	@echo -e "Generating consensus levels plot ... \c"
	@pickout --no-header LOD full < $< > $@
	@echo "done."

Results/c-sub-levels.plot : Results/consensus
	@echo -e "Generating consensus sub-levels plot ... \c"
	@pickout --no-header LOD sub\#1 < $< > $@
	@echo "done."

Results/%.tree : Results/%.topology Work/trees
	@echo "Calculating branch lengths for $* tree."
	@tree-mean-lengths $< --safe --skip ${SKIP} ${SIZE:%=--max %} < Work/trees > $@ 2>/dev/null

Results/MAP.topology: Results/consensus Work/setup
	@pickout --no-header MAP-0 < $< > $@

Work/MAP-unordered.fasta: Work/MAP Work/setup
	@echo -e "Locating MAP alignment... \c"
	@alignment-find < Work/MAP > $@
	@echo "done."

Work/median-unordered.fasta: Work/samples-${SKIP}+${SIZE}
	alignment-median --max-alignments 2000 --type splits < $< > $@ 2>median.err

Results/%.fasta: Results/c50.tree Work/%-unordered.fasta
	@alignment-reorder Work/$*-unordered.fasta Results/c50.tree > $@ 2>/dev/null

Results/au-graph.plot Work/consensus-unordered.fasta: Work/samples-${SKIP}+${SIZE}
	@echo -e "Computing consensus alignment ... \c"
	@alignment-consensus --cutoff=0.5 --uncertainty=Results/au-graph.plot < Work/samples-${SKIP}+${SIZE} > $@ 2>/dev/null
	@echo "done."

Results/%-AU.prob: Results/%.fasta Work/samples-${SKIP}+${SIZE} Results/MAP.tree
	@echo -e "Calculating AU probabilities for $* alignment ... \c"
	@alignment-gild Results/$*.fasta Results/MAP.tree --max-alignments 500 < Work/samples-${SKIP}+${SIZE} > $@ 2> Work/$*-AU.err
	@echo "done."

Results/%-AU.html: Results/%-AU.prob Results/%.fasta
	@echo -e "Drawing $* alignment plot ... \c"
	@alignment-draw Results/$*.fasta Results/$*-AU.prob --width 80 > $@
	@echo "done."

Results/%-AU2.html: Results/%-AU.prob Results/%.fasta
	@echo -e "Drawing $* alignment plot (2) ... \c"
	@alignment-draw Results/$*.fasta Results/$*-AU.prob --width 80 --color-scheme AA+contrast+fade+fade+fade+fade > $@
	@echo "done."

Results/c%.topology: Results/consensus Work/setup
	@pickout --no-header $*-consensus < $< > $@ 
	@test -s $@

Results/c%.mtree: Results/consensus Work/setup
	@pickout --no-header --multi-line $*-consensus < $< > $@ 
	@test -s $@

Results/c%.dot: Results/c%.mtree
	@draw-graph $< > $@

Results/c%.ps: Results/c%.dot
	@neato -Tps $< > $@

Mixing/partitions.SRQ: Work/trees Work/partitions.pred
	@echo -e "Generating SRQ plot for c50 partitions... \c"
	@trees-to-SRQ Work/partitions.pred --skip ${SKIP} ${SIZE:%=--max %} < Work/trees> $@
	@echo "done."

Mixing/%.SRQ: Work/trees Results/%.topology
	@echo -e "Generating SRQ plot for $* tree... \c"
	@trees-to-SRQ Results/$*.topology --skip ${SKIP} ${SIZE:%=--max %} < Work/trees> $@
	@echo "done."

Mixing/sum.plot: Work/trees Work/partitions.pred
	@echo -e "Generating sum plot for c50 partitions... \c"
	@trees-to-SRQ Work/partitions.pred  --skip ${SKIP} ${SIZE:%=--max %} < Work/trees--mode sum --no-scale-x > $@
	@echo "done."

Mixing/sumi.plot: Work/trees Work/partitions.pred
	@echo -e "Generating inverse sum plot for c50 partitions... \c"
	@trees-to-SRQ Work/partitions.pred  --invert --skip ${SKIP} ${SIZE:%=--max %} < Work/trees --mode sum --no-scale-x > $@
	@echo "done."

Work/Likelihood: Work/p Work/setup
	@stats-select --no-header likelihood < Work/p > $@

Work/Pmarg: Work/Likelihood.from.${SKIP}+${SIZE}
	@echo -e "Calculating marginal likelihood... \c"
	@model_P < $< > $@ 2>/dev/null
	@echo "done."

clean:
	rm -rf Work/ Results/ Mixing/

%.fasta: %.phy
	@alignment-convert --output fasta < $< > $@

%.phy: %.fasta
	@alignment-convert --output phylip < $< > $@
