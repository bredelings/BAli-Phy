SUBDIRS = src/

bin_SCRIPTS = bin/bali-phy-sge bin/genreport.pl bin/pickout.pl

docdir = $(datadir)/doc/@PACKAGE@
doc_DATA = bin/GNUmakefile doc/README.xhtml doc/Doxyfile # doc/README.pdf


EXTRA_HEADERS = @top_srcdir@/include/tnt/tnt_array1d.h \
                @top_srcdir@/include/tnt/tnt_array2d.h \
                @top_srcdir@/include/tnt/tnt_math_utils.h \
                @top_srcdir@/include/tnt/jama_eig.h

EXTRA_DIST = $(doc_DATA) $(EXTRA_HEADERS) $(bin_SCRIPTS)
