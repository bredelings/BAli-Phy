if NO_SYSTEM_BOOST
  MAYBE_BOOST = boost/
endif

SUBDIRS = $(MAYBE_BOOST) src/ 

bin_SCRIPTS = scripts/bali-phy-sge scripts/pairwise-alignment-distances scripts/fixedpt-alignment-distances scripts/plot-path-graph.R scripts/bp-analyze.pl

docdir = $(datadir)/doc/@PACKAGE@
doc_DATA = scripts/GNUmakefile doc/README.xhtml doc/docbook.css # doc/README.pdf

stuffdir = $(datadir)/@PACKAGE@/
nobase_stuff_DATA = Data/WAG.dat Data/WAGSTAR.dat Data/JTT.dat Data/PAM.dat \
                Data/standard-code.txt Data/mt-invert-code.txt \
		Data/mt-protozoan-code.txt Data/mt-vert-code.txt \
		Data/mt-yeast-code.txt Data/C20.dat Data/C10.dat

examplesdir = $(datadir)/@PACKAGE@/
nobase_examples_DATA = examples/EF-Tu/5d.fasta examples/EF-Tu/12d.fasta examples/EF-Tu/48b.fasta \
                examples/5S-rRNA/5d.fasta examples/5S-rRNA/5d-muscle.fasta \
		examples/5S-rRNA/5d-clustalw.fasta examples/5S-rRNA/5d-clustalw.dnd \
                examples/5S-rRNA/25.fasta examples/5S-rRNA/25-muscle.fasta \
		examples/5S-rRNA/25-clustalw.fasta examples/5S-rRNA/25-clustalw.dnd \
                examples/5S-rRNA/48.fasta examples/5S-rRNA/48-muscle.fasta \
		examples/5S-rRNA/48-clustalw.fasta examples/5S-rRNA/48-clustalw.dnd \
		examples/Globins/many.fasta examples/Globins/few.fasta \
		examples/Globins/few-clustalw.fasta examples/Globins/few-clustalw.dnd \
		examples/Globins/few-muscle.fasta examples/Enolase/enolase-38.fasta \
		examples/Enolase/enolase-38-trimmed.fasta

EXTRA_HEADERS = @top_srcdir@/include/tnt/tnt_array1d.h \
                @top_srcdir@/include/tnt/tnt_array2d.h \
                @top_srcdir@/include/tnt/tnt_math_utils.h \
                @top_srcdir@/include/tnt/jama_eig.h

EXTRA_DIST = $(doc_DATA) $(EXTRA_HEADERS) $(bin_SCRIPTS) $(nobase_stuff_DATA) \
             $(nobase_examples_DATA) doc/Doxyfile 

dist-hook:
	rm -rf `find $(distdir) -name .svn`
	rm -rf `find $(distdir) -name TODO`
	if test -e "$(srcdir)/src/revision.H"; then cp $(srcdir)/src/revision.H $(distdir)/src/revision.H ; fi
