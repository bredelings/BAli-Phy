# Help aclocal find the pkg.m4 file in the m4/ directory
ACLOCAL_AMFLAGS = -I m4

# Should we build our own copy of some BOOST libraries?
if NO_SYSTEM_BOOST
  MAYBE_BOOST = boost/
endif

# Build in these subdirectories
SUBDIRS = $(MAYBE_BOOST) src/ 

# Install these scripts in bin/
bin_SCRIPTS = scripts/bali-phy-sge scripts/bp-analyze.pl scripts/compare-runs.R

# Used to build, but not distributed
noinst_SCRIPTS = scripts/git_version.sh

# Distributed in the doc/ directory
nobase_doc_SCRIPTS = scripts/plot-path-graph.R \
		scripts/fixedpt-alignment-distances scripts/pairwise-alignment-distances

docdir = $(datadir)/doc/@PACKAGE@
doc_DATA = doc/README.html doc/README.xhtml doc/docbook.css doc/README.pdf

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

EXTRA_DIST = $(doc_DATA) $(EXTRA_HEADERS) $(bin_SCRIPTS) $(noinst_SCRIPTS) \
             $(nobase_stuff_DATA) $(nobase_examples_DATA) doc/Doxyfile 

dist-hook:
	rm -rf `find $(distdir) -name TODO`
