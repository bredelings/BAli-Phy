SUBDIRS = boost/ src/ 

bin_SCRIPTS = bin/bali-phy-sge bin/genreport.pl bin/pickout.pl

docdir = $(datadir)/doc/@PACKAGE@
doc_DATA = bin/GNUmakefile doc/README.xhtml # doc/README.pdf

stuffdir = $(datadir)/@PACKAGE@/
nobase_stuff_DATA = Data/wag.dat Data/wagstar.dat Data/jtt.dat Data/pam.dat \
                Data/standard-code.txt Data/mt-invert-code.txt \
		Data/mt-protozoan-code.txt Data/mt-vert-code.txt \
		Data/mt-yeast-code.txt

examplesdir = $(datadir)/@PACKAGE@/
nobase_examples_DATA = examples/EF-Tu/5d.fasta examples/EF-Tu/12d.fasta examples/EF-Tu/48b.fasta \
                examples/5S-rRNA/5d.fasta examples/5S-rRNA/21.fasta 

EXTRA_HEADERS = @top_srcdir@/include/tnt/tnt_array1d.h \
                @top_srcdir@/include/tnt/tnt_array2d.h \
                @top_srcdir@/include/tnt/tnt_math_utils.h \
                @top_srcdir@/include/tnt/jama_eig.h

EXTRA_DIST = $(doc_DATA) $(EXTRA_HEADERS) $(bin_SCRIPTS) $(nobase_stuff_DATA) \
             $(nobase_examples_DATA) doc/Doxyfile 

dist-hook:
	rm -rf `find $(distdir) -name .svn`
	rm -rf `find $(distdir) -name TODO`
