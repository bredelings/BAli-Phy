SHELL=/bin/sh

.DELETE_ON_ERROR: 

.INTERMEDIATE: pSnames pSreport pInames pIreport

.PRECIOUS: %.prob %.from.${SKIP}+${SIZE}

analysis: Report tc tree.plot MAP.tree MAP.phy MAP.fasta # AU.html
	echo "Started analyzing from iteration ${SKIP}" > analysis
	echo "Analyzed $$(wc -l trees.from.${SKIP}+${SIZE}) " >> analysis
	echo "Finished on date $$(date)" >> analysis

Report:  header pSreport pIreport MAP.tree
	cat $^ > $@

header: out Pmarg
	{ ~/bin/genreport.pl; \
	echo ; } > $@

samples: out
	cut-range key=iterations skip=${SKIP} size=${SIZE} < $< > $@

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

MAP-unordered.phy: out
	findalign < MAP > $@

%-chopped.phy: %.phy
	alignment-chop-internal align=$< > $@

MAP.phy: MAP.tree-trunc MAP-unordered-chopped.phy
	alignment-reorder align=MAP-unordered-chopped.phy tree=MAP.tree-trunc > $@ 2>/dev/null

AU.prob: samples MAP.tree-trunc MAP-unordered.phy
	alignment-gild align=MAP-unordered.phy tree=MAP.tree-trunc maxalignments=500 < samples > $@ 2>/dev/null

%.tree-trunc: %.tree
	tree-names-trunc tree=$< > $@

%.html: %.prob MAP.tree MAP.phy
	alignment-draw width=80 tree=MAP.tree-trunc align=MAP.phy colors=$< > $@

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

%.fasta: %.phy
	phy_to_fasta < $< > $@
