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

modulesdir = $(libdir)/@PACKAGE@/modules/
modules_DATA = modules/Distributions.hs modules/PopGen.hs modules/Prelude.hs modules/Range.hs modules/SModel.hs \
		modules/Tree.hs

# Distributed in the doc/ directory
docdir = $(datadir)/doc/@PACKAGE@
doc_DATA = doc/README.html doc/README.xhtml doc/docbook.css doc/README.pdf \
		scripts/plot-path-graph.R \
		scripts/fixedpt-alignment-distances \
		scripts/pairwise-alignment-distances


examplesdir = $(datadir)/@PACKAGE@/
nobase_examples_DATA = examples/EF-Tu/5d.fasta examples/EF-Tu/12d.fasta examples/EF-Tu/48b.fasta \
		\
		examples/5S-rRNA/5d.fasta examples/5S-rRNA/5d-muscle.fasta \
		examples/5S-rRNA/5d-clustalw.fasta examples/5S-rRNA/5d-clustalw.dnd \
                examples/5S-rRNA/25.fasta examples/5S-rRNA/25-muscle.fasta \
		examples/5S-rRNA/25-clustalw.fasta examples/5S-rRNA/25-clustalw.dnd \
                examples/5S-rRNA/48.fasta examples/5S-rRNA/48-muscle.fasta \
		examples/5S-rRNA/48-clustalw.fasta examples/5S-rRNA/48-clustalw.dnd \
		\
		examples/Feng-Doolittle-1987/globins.fasta \
		examples/Feng-Doolittle-1987/globins-muscle.fasta \
		examples/Feng-Doolittle-1987/globins-mafft.fasta \
		examples/Feng-Doolittle-1987/globins-fsa.fasta \
		examples/Feng-Doolittle-1987/globins-prank.1.fasta \
		examples/Feng-Doolittle-1987/globins-prank.1.dnd \
		examples/Feng-Doolittle-1987/globins-prank.2.fasta \
		examples/Feng-Doolittle-1987/globins-prank.2.dnd \
		examples/Feng-Doolittle-1987/globins-bali-phy-C20-gamma4.fasta \
		examples/Feng-Doolittle-1987/globins-bali-phy-C20-gamma4-c50.PP.tree \
		examples/Feng-Doolittle-1987/globins-bali-phy-C20-gamma4.MAP.tree \
		examples/Feng-Doolittle-1987/sod.fasta \
		examples/Feng-Doolittle-1987/sod-muscle.fasta \
		examples/Feng-Doolittle-1987/sod-mafft.fasta \
		examples/Feng-Doolittle-1987/sod-fsa.fasta \
		examples/Feng-Doolittle-1987/sod-prank.1.fasta \
		examples/Feng-Doolittle-1987/sod-prank.1.dnd \
		examples/Feng-Doolittle-1987/sod-prank.2.fasta \
		examples/Feng-Doolittle-1987/sod-prank.2.dnd \
		examples/Feng-Doolittle-1987/sod-bali-phy-C20-gamma4.fasta \
		examples/Feng-Doolittle-1987/sod-bali-phy-C20-gamma4-c50.PP.tree \
		examples/Feng-Doolittle-1987/sod-bali-phy-C20-gamma4.MAP.tree \
		examples/Feng-Doolittle-1987/v-src.fasta \
		examples/Feng-Doolittle-1987/v-src-muscle.fasta \
		examples/Feng-Doolittle-1987/v-src-mafft.fasta \
		examples/Feng-Doolittle-1987/v-src-fsa.fasta \
		\
		examples/Globins/many.fasta \
		examples/Globins/few.fasta \
		examples/Globins/few-clustalw.fasta \
		examples/Globins/few-clustalw.dnd \
		examples/Globins/few-muscle.fasta \
		examples/Globins/bglobin.fasta \
		\
		examples/Miklos-2008/annexin.fasta \
		examples/Miklos-2008/haloperoxidase.fasta \
		examples/Miklos-2008/lectin.fasta \
		examples/Miklos-2008/legume_lectin.fasta \
		examples/Miklos-2008/lysozyme.fasta \
		examples/Miklos-2008/papain.fasta \
		examples/Miklos-2008/parvalbumin-like.fasta \
		examples/Miklos-2008/rr_receiver.fasta \
		examples/Miklos-2008/Src_homology_2_domains.fasta \
		examples/Miklos-2008/Starch_binding_domain.fasta \
		examples/Miklos-2008/xylose_isomerase.fasta \
		\
		examples/Enolase/enolase-38.fasta \
		examples/Enolase/enolase-38-trimmed.fasta \
		\
		examples/HIV/chain-2005/env-clustal-codons.fasta \
		examples/HIV/chain-2005/env-clustal.fasta \
		examples/HIV/chain-2005/env.fasta \
		examples/HIV/chain-2005/pol-clustal.fasta \
		examples/HIV/chain-2005/pol.fasta


EXTRA_HEADERS = @top_srcdir@/include/tnt/tnt_array1d.h \
                @top_srcdir@/include/tnt/tnt_array2d.h \
                @top_srcdir@/include/tnt/tnt_math_utils.h \
                @top_srcdir@/include/tnt/jama_eig.h

EXTRA_DIST = $(doc_DATA) $(EXTRA_HEADERS) $(bin_SCRIPTS) $(noinst_SCRIPTS) \
             $(nobase_doc_SCRIPTS) $(nobase_examples_DATA) doc/Doxyfile $(modules_DATA)

dist-hook:
	rm -rf `find $(distdir) -name TODO`
