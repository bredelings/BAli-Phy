SHELL=/bin/sh

TREES=c50 c66 c80 c90 c95 c99 c100 MAP

all: Results/analysis

#.DELETE_ON_ERROR: 

#.INTERMEDIATE: pSnames pSreport pInames pIreport

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
	@mkdir -p Results/
	@touch Work/setup
	@echo "done."

Results/analysis: Results/Report Work/tc Results/SRQ.plot Results/sum.plot \
		  ${TREES:%=Results/%.tree} ${TREES:%=Results/%.topology} \
                  Results/sumi.plot Work/trees.from.${SKIP}+${SIZE} \
                  Results/MAP.phy Results/MAP.fasta # MAP-AU.html median-AU.html
	@echo "Analyzed:  $$(wc -l < Work/trees.from.${SKIP}+${SIZE}) trees from iteration ${SKIP}." > Results/analysis
	@echo "Finished:  $$(date)" >> Results/analysis
	@echo 
	@cat Results/analysis
	@echo 

Results/Report:  Work/header Work/pSreport Work/pIreport Results/MAP.topology
	@cat $^ > $@

Work/header: out Work/Pmarg
	@{ ~/bin/genreport.pl; echo ; } > $@

Work/samples-${SKIP}+${SIZE}: Work/out
	@cut-range --skip ${SKIP} ${SIZE:%=--size %} < $< > $@

%.from.${SKIP}: %
	@tail -n +${SKIP} $< > $@

Work/%: % Work/setup
	@cp $* Work/$*

Work/%.from.${SKIP}+${SIZE}: Work/%.from.${SKIP}
	@head -n ${SIZE} $< > $@ 

Work/%.from.${SKIP}+: Work/%.from.${SKIP}
	@cp $< $@ 

Work/tc : Work/trees
	@echo -e "Summarizing topology distribution... \c";
	@tree-dist-compare --skip ${SKIP} ${SIZE:%=--max %} $< --consensus 0.5,0.66,0.8,0.9,0.95,0.99,1.0 --sub-partitions > $@
	@echo "done."

# If there are no Substitution parameters, don't bail out

Work/pSnames : pS Work/setup
	@getnames.pl < $< > $@

Work/pSall : Work/pSnames Work/pS.from.${SKIP}+${SIZE}
	@for i in $$(cat $<); do \
	  pickout "$$i" < Work/pS.from.${SKIP}+${SIZE} > Work/"$$i"; \
	done

Work/pSreport: Work/pSnames Work/pSall
	@echo -e "Analyzing substitution parameters... \c"
	@rm -f $@; \
	for i in $$(cat $<); do \
	  statreport "$$i" < Work/"$$i" >> $@ 2>/dev/null; \
	  echo "   " >> $@ ; \
	done; \
	echo "   " >> $@ ;
	@echo "done."

# If there are no Indel parameters, don't bail out
Work/pInames : pI Work/setup
	@getnames.pl < $< > $@

Work/pIall : Work/pInames Work/pI.from.${SKIP}+${SIZE}
	@for i in $$(cat $<); do \
	  pickout "$$i" < Work/pI.from.${SKIP}+${SIZE} > Work/"$$i"; \
	done

Work/pIreport: Work/pInames Work/pIall
	@echo -e "Analyzing indel parameters... \c"
	@rm -f $@; \
	for i in $$(cat $<); do \
	  statreport "$$i" < Work/"$$i" >> $@ 2>/dev/null; \
	  echo "   " >> $@ ; \
	done; \
	echo "   " >> $@ ; 
	@echo "done."

Results/%.tree : Results/%.topology Work/trees
	@echo "Calculating branch lengths for $* tree."
	@tree-mean-lengths $< --skip ${SKIP} ${SIZE:%=--max %} < Work/trees > $@ 2>/dev/null

Results/MAP.topology: Work/tc Work/setup
	@pickout MAP-0 < $< > $@

Work/MAP-unordered.fasta: out Work/setup
	@echo -e "Locating MAP alignment... \c"
	@alignment-find < MAP > $@
	@echo "done."

Work/median-unordered.fasta: Work/samples-${SKIP}+${SIZE}
	alignment-median --max-alignments 2000 --type splits < $< > $@ 2>median.err

Results/%.fasta: Results/c50.tree Work/%-unordered.fasta
	@alignment-reorder Work/$*-unordered.fasta Results/c50.tree > $@ 2>/dev/null

Results/%-AU.prob: Work/%-unordered.fasta Work/samples-${SKIP}+${SIZE} Results/MAP.tree
	alignment-gild Work/$*-unordered.fasta Results/MAP.tree --max-alignments 500 < Work/samples-${SKIP}+${SIZE} > $@ 2> Work/$*-AU.err

Results/%-AU.html: Results/%-AU.prob Results/%.fasta
	alignment-draw Results/$*.fasta Results/$*-AU.prob --width 100 > $@

Results/c%.topology: Work/tc Work/setup
	@pickout $*-consensus-0 < $< > $@ 
	@test -s $@

Results/SRQ.plot: Work/trees Results/c50.topology
	@echo -e "Generating SRQ plot for c50 partitions... \c"
	@tree-to-srq Results/c50.topology  --skip ${SKIP} ${SIZE:%=--max %} < Work/trees > $@
	@echo "done."

Results/sum.plot: Work/trees Results/c50.topology
	@echo -e "Generating sum plot for c50 partitions... \c"
	@tree-to-srq Results/c50.topology  --skip ${SKIP} ${SIZE:%=--max %} < Work/trees --mode sum --no-scale-x > $@
	@echo "done."

Results/sumi.plot: Work/trees Results/c50.topology
	@echo -e "Generating inverse sum plot for c50 partitions... \c"
	@tree-to-srq Results/c50.topology  --invert --skip ${SKIP} ${SIZE:%=--max %} < Work/trees --mode sum --no-scale-x > $@
	@echo "done."

Work/Likelihood: Pr Work/setup
	@pickout likelihood < Pr  > $@

Work/prior: Pr Work/setup
	@pickout prior < Pr  > $@

Work/Pmarg: Work/Likelihood.from.${SKIP}+${SIZE}
	@echo -e "Calculating marginal likelihood... \c"
	@model_P < $< > $@ 2>/dev/null
	@echo "done."

clean:
	rm -rf Work/ Results/

%.fasta: %.phy
	@alignment-convert --output fasta < $< > $@

%.phy: %.fasta
	@alignment-convert --output phylip < $< > $@
