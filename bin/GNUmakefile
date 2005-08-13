SHELL=/bin/sh

.DELETE_ON_ERROR: 

.INTERMEDIATE: pSnames pSreport pInames pIreport

.PRECIOUS: %.prob %.from.${SKIP}+${SIZE}

analysis: Report tc SRQ.plot sum.plot c50.tree MAP.tree MAP.phy MAP.fasta # MAP-AU.html median-AU.html
	echo "Started analyzing from iteration ${SKIP}" > analysis
	echo "Analyzed $$(wc -l trees.from.${SKIP}+${SIZE}) " >> analysis
	echo "Finished on date $$(date)" >> analysis

Report:  header pSreport pIreport MAP.tree
	cat $^ > $@

header: out Pmarg
	{ ~/bin/genreport.pl; \
	echo ; } > $@

samples-${SKIP}+${SIZE}: out
	cut-range --skip ${SKIP} ${SIZE:--size %} < $< > $@

%.from.${SKIP}: %
	tail -n +${SKIP} $< > $@

%.from.${SKIP}+${SIZE}: %.from.${SKIP}
	head -n ${SIZE} $< > $@ 

%.from.${SKIP}+: %.from.${SKIP}
	cp $< $@ 

tc : trees.from.${SKIP}+${SIZE}
	tree-dist-compare $< --consensus 0.5 --consensus 0.75 --consensus 0.9 --consensus 0.99 > $@

# If there are no Substitution parameters, don't bail out

pSnames : pS
	getnames.pl < $< > $@

pSall : pSnames	pS.from.${SKIP}+${SIZE}
	for i in $$(cat $<); do \
	  pickout "$$i" < pS.from.${SKIP}+${SIZE} > "$$i"; \
	done

pSreport: pSnames pSall
	rm -f $@; \
	for i in $$(cat $<); do \
	  statreport "$$i" < "$$i" >> $@ ; \
	  echo "   " >> $@ ; \
	done; \
	echo "   " >> $@ ;

# If there are no Indel parameters, don't bail out
pInames : pI
	getnames.pl < $< > $@

pIall : pInames	pI.from.${SKIP}+${SIZE}
	for i in $$(cat $<); do \
	  pickout "$$i" < pI.from.${SKIP}+${SIZE} > "$$i"; \
	done

pIreport: pInames pIall
	rm -f $@; \
	for i in $$(cat $<); do \
	  statreport "$$i" < "$$i" >> $@ ; \
	  echo "   " >> $@ ; \
	done; \
	echo "   " >> $@ ; 

%.tree : %.topology trees.from.${SKIP}+${SIZE}
	tree-mean-lengths $< < trees.from.${SKIP}+${SIZE} > $@

MAP.tree: tc
	pickout MAPtree_0 < $< > $@

MAP-unordered.fasta: out
	alignment-find < MAP > $@

median-unordered.fasta: samples-${SKIP}+${SIZE}
	alignment-median --max-alignments 2000 --type splits < $< > $@ 2>median.err

%.fasta: c50.tree %-unordered.fasta
	alignment-reorder $*-unordered.fasta c50.tree > $@ 2>/dev/null

%-AU.prob: %-unordered.fasta samples-${SKIP}+${SIZE} MAP.tree
	alignment-gild $*-unordered.fasta MAP.tree --max-alignments 500 < samples-${SKIP}+${SIZE} > $@ 2>$*-AU.err

%-AU.html: %-AU.prob %.fasta
	alignment-draw $*.fasta $*-AU.prob --width 120 > $@

c50.topology: tc
	pickout consensus0 < tc > c50.topology

SRQ.plot: trees.from.${SKIP}+${SIZE} c50.tree
	tree-to-srq c50.tree < trees.from.${SKIP}+${SIZE} > SRQ.plot

sum.plot: trees.from.${SKIP}+${SIZE} c50.tree
	tree-to-srq c50.tree < trees.from.${SKIP}+${SIZE} --mode sum --no-scale-x > sum.plot

Likelihood: Pr
	pickout likelihood < Pr  > $@

prior: Pr
	pickout prior < Pr  > $@

Pmarg: Likelihood.from.${SKIP}+${SIZE}
	model_P < $< > $@

clean:
	rm pS* pI* Report header samples tc trees MAP.tree analysis

%.fasta: %.phy
	alignment-convert --output fasta < $< > $@

%.phy: %.fasta
	alignment-convert --output phylip < $< > $@
