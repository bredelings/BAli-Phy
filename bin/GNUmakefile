SHELL=/bin/sh

.DELETE_ON_ERROR: 

.INTERMEDIATE: pSnames pSreport pInames pIreport

.PRECIOUS: %.prob %.from.${SKIP}+${SIZE}

analysis: Report tc tree.plot MAP.tree MAP.phy MAP.fasta # MAP-AU.html median-AU.html
	echo "Started analyzing from iteration ${SKIP}" > analysis
	echo "Analyzed $$(wc -l trees.from.${SKIP}+${SIZE}) " >> analysis
	echo "Finished on date $$(date)" >> analysis

Report:  header pSreport pIreport MAP.tree
	cat $^ > $@

header: out Pmarg
	{ ~/bin/genreport.pl; \
	echo ; } > $@

samples-${SKIP}+${SIZE}: out
	cut-range key=iterations skip=${SKIP} ${SIZE:size=%} < $< > $@

%.from.${SKIP}: %
	tail -n +${SKIP} $< > $@

%.from.${SKIP}+${SIZE}: %.from.${SKIP}
	head -n ${SIZE} $< > $@ 

%.from.${SKIP}+: %.from.${SKIP}
	cp $< $@ 

tc : trees.from.${SKIP}+${SIZE}
	tree-dist-compare files=$< > $@

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

MAP.tree: tc
	pickout MAPtree_0 < $< > $@

MAP-unordered.fasta: out
	findalign < MAP > $@

median-unordered.fasta: samples-${SKIP}+${SIZE}
	alignment-median maxalignments=2000 type=splits < $< > $@ 2>median.err

%.fasta: MAP.tree %-unordered.fasta
	alignment-reorder align=$*-unordered.fasta tree=MAP.tree > $@ 2>/dev/null

%-AU.prob: %-unordered.fasta samples-${SKIP}+${SIZE} MAP.tree
	alignment-gild align=$*-unordered.fasta tree=MAP.tree maxalignments=500 < samples-${SKIP}+${SIZE} > $@ 2>$*-AU.err

%-AU.html: %-AU.prob MAP.tree %.fasta
	alignment-draw width=120 tree=MAP.tree align=$*.fasta AU-file=$*-AU.prob > $@

tree.srq: trees MAP.tree 
	tree-to-srq tree=MAP.tree < trees > tree.srq

tree.plot: tree.srq
	srq-to-plot scale=no < $< > $@

Likelihood: Pr
	pickout likelihood < Pr  > $@

prior: Pr
	pickout prior < Pr  > $@

Pmarg: Likelihood.from.${SKIP}+${SIZE}
	model_P < $< > $@

clean:
	rm pS* pI* Report header samples tc trees MAP.tree analysis

#%.fasta: %.phy
#	phy_to_fasta < $< > $@
