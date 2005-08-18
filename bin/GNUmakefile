>SHELL=/bin/sh

.DELETE_ON_ERROR: 

.INTERMEDIATE: pSnames pSreport pInames pIreport

.PRECIOUS: %.prob %.from.${SKIP}+${SIZE}

all: Results/analysis

Work:
	mkdir -p Work

Results:
	mkdir -p Results/

Results/analysis: Results/Report Work/tc Results/SRQ.plot Results/sum.plot Results/c50.tree \
                  Results/MAP.tree Results/MAP.phy Results/MAP.fasta # MAP-AU.html median-AU.html
	echo "Started analyzing from iteration ${SKIP}" > analysis
	echo "Analyzed $$(wc -l trees.from.${SKIP}+${SIZE}) " >> analysis
	echo "Finished on date $$(date)" >> Results/analysis

Results/Report:  Work/header Work/pSreport Work/pIreport Results/MAP.topology
	cat $^ > $@

Work/header: out Work/Pmarg
	{ ~/bin/genreport.pl; \
	echo ; } > $@

Work/samples-${SKIP}+${SIZE}: out
	cut-range --skip ${SKIP} ${SIZE:%=--size %} < $< > $@

Work/%.from.${SKIP}: Work/%
	tail -n +${SKIP} $< > $@

Work/%.from.${SKIP}: % Work
	tail -n +${SKIP} $< > $@

Work/%.from.${SKIP}+${SIZE}: Work/%.from.${SKIP}
	head -n ${SIZE} $< > $@ 

Work/%.from.${SKIP}+: Work/%.from.${SKIP}
	cp $< $@ 

Work/tc : trees Work
	tree-dist-compare --skip ${SKIP} ${SIZE:%=--max %} $< --consensus 0.5,0.66,0.8,0.9,0.95,0.99 --sub-partitions > $@

# If there are no Substitution parameters, don't bail out

Work/pSnames : pS Work
	getnames.pl < $< > $@

Work/pSall : Work/pSnames Work/pS.from.${SKIP}+${SIZE}
	for i in $$(cat $<); do \
	  pickout "$$i" < Work/pS.from.${SKIP}+${SIZE} > Work/"$$i"; \
	done

Work/pSreport: Work/pSnames Work/pSall
	rm -f $@; \
	for i in $$(cat $<); do \
	  statreport "$$i" < Work/"$$i" >> $@ ; \
	  echo "   " >> $@ ; \
	done; \
	echo "   " >> $@ ;

# If there are no Indel parameters, don't bail out
Work/pInames : pI Work
	getnames.pl < $< > $@

Work/pIall : Work/pInames Work/pI.from.${SKIP}+${SIZE}
	for i in $$(cat $<); do \
	  pickout "$$i" < Work/pI.from.${SKIP}+${SIZE} > Work/"$$i"; \
	done

Work/pIreport: Work/pInames Work/pIall
	rm -f $@; \
	for i in $$(cat $<); do \
	  statreport "$$i" < Work/"$$i" >> $@ ; \
	  echo "   " >> $@ ; \
	done; \
	echo "   " >> $@ ; 

Results/%.tree : Results/%.topology Work/trees.from.${SKIP}+${SIZE}
	tree-mean-lengths $< < Work/trees.from.${SKIP}+${SIZE} > $@

Results/MAP.topology: Work/tc Results
	pickout MAPtree-0 < $< > $@

Work/MAP-unordered.fasta: out Work
	alignment-find < MAP > $@

Work/median-unordered.fasta: Work/samples-${SKIP}+${SIZE}
	alignment-median --max-alignments 2000 --type splits < $< > $@ 2>median.err

Results/%.fasta: Results/c50.tree Work/%-unordered.fasta
	alignment-reorder Work/$*-unordered.fasta Results/c50.tree > $@ 2>/dev/null

Results/%-AU.prob: Work/%-unordered.fasta Work/samples-${SKIP}+${SIZE} Results/MAP.tree
	alignment-gild Work/$*-unordered.fasta Results/MAP.tree --max-alignments 500 < Work/samples-${SKIP}+${SIZE} > $@ 2> Work/$*-AU.err

Results/%-AU.html: Results/%-AU.prob Results/%.fasta
	alignment-draw Results/$*.fasta Results/$*-AU.prob --width 120 > $@

Results/c50.topology: Work/tc Results
	pickout consensus-0 < $< | head -n 1 > $@ 

Results/SRQ.plot: trees Results/c50.topology
	tree-to-srq Results/c50.topology  --skip ${SKIP} ${SIZE:%=--max %} < trees > $@

Results/sum.plot: trees Results/c50.topology
	tree-to-srq Results/c50.topology  --skip ${SKIP} ${SIZE:%=--max %} < trees --mode sum --no-scale-x > $@

Work/Likelihood: Pr Work
	pickout likelihood < Pr  > $@

Work/prior: Pr Work
	pickout prior < Pr  > $@

Work/Pmarg: Work/Likelihood.from.${SKIP}+${SIZE}
	model_P < $< > $@

clean:
	rm pS* pI* Report header samples tc trees MAP.tree analysis

%.fasta: %.phy
	alignment-convert --output fasta < $< > $@

%.phy: %.fasta
	alignment-convert --output phylip < $< > $@
