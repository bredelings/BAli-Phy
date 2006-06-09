SUBDIRS = src/

bin_SCRIPTS = bin/bali-phy-sge bin/genreport.pl bin/pickout.pl

docdir = $(datadir)/doc/@PACKAGE@
doc_DATA = bin/GNUmakefile doc/README.xhtml doc/Doxyfile # doc/README.pdf

stuffdir = $(datadir)/@PACKAGE@
nobase_stuff_DATA = Data/wag.dat Data/wagstar.dat Data/jtt.dat Data/pam.dat \
             Data/EF-Tu/5d.fasta Data/EF-Tu/12d.fasta Data/EF-Tu/48b.fasta \
             Data/5S-rRNA/5d.fasta Data/5S-rRNA/21.fasta

EXTRA_HEADERS = @top_srcdir@/include/tnt/tnt_array1d.h \
                @top_srcdir@/include/tnt/tnt_array2d.h \
                @top_srcdir@/include/tnt/tnt_math_utils.h \
                @top_srcdir@/include/tnt/jama_eig.h

EXTRA_DIST = $(doc_DATA) $(EXTRA_HEADERS) $(bin_SCRIPTS) $(nobase_stuff_DATA)
