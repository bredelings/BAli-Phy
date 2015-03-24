# Help aclocal find the pkg.m4 file in the m4/ directory
ACLOCAL_AMFLAGS = -I m4

# Should we build our own copy of some BOOST libraries?
if NO_SYSTEM_BOOST
  MAYBE_BOOST = boost/
endif

# Build in these subdirectories
SUBDIRS = $(MAYBE_BOOST) src/ 

# Install these scripts in bin/
bin_SCRIPTS = scripts/bali-phy-sge scripts/bp-analyze.pl scripts/compare-runs.R scripts/compare-runs2.R \
		scripts/tree-plot1.R scripts/tree-plot2.R scripts/tree-plot3.R scripts/bali-phy-pkg

# Used to build, but not distributed
noinst_SCRIPTS = scripts/git_version.sh

modulesdir = $(libdir)/@PACKAGE@/
nobase_modules_DATA = modules/Distributions.hs modules/PopGen.hs modules/Prelude.hs modules/Range.hs modules/SModel.hs \
		modules/Tree.hs modules/Alphabet.hs modules/Parameters.hs modules/Alignment.hs \
		modules/MCMC.hs modules/IModel.hs modules/System/Environment.hs modules/Data/ReadFile.hs

# Distributed in the doc/ directory
docdir = $(datadir)/doc/@PACKAGE@
doc_DATA = doc/README.html doc/README.xhtml doc/docbook.css doc/README.pdf \
		scripts/plot-path-graph.R \
		scripts/fixedpt-alignment-distances \
		scripts/pairwise-alignment-distances


examplesdir = $(datadir)/@PACKAGE@/
nobase_examples_DATA = examples/Sequences/EF-Tu/5d.fasta examples/Sequences/EF-Tu/12d.fasta examples/Sequences/EF-Tu/48b.fasta \
		\
		examples/Sequences/5S-rRNA/5d.fasta examples/Sequences/5S-rRNA/5d-muscle.fasta \
		examples/Sequences/5S-rRNA/5d-clustalw.fasta examples/Sequences/5S-rRNA/5d-clustalw.dnd \
                examples/Sequences/5S-rRNA/25.fasta examples/Sequences/5S-rRNA/25-muscle.fasta \
		examples/Sequences/5S-rRNA/25-clustalw.fasta examples/Sequences/5S-rRNA/25-clustalw.dnd \
                examples/Sequences/5S-rRNA/48.fasta examples/Sequences/5S-rRNA/48-muscle.fasta \
		examples/Sequences/5S-rRNA/48-clustalw.fasta examples/Sequences/5S-rRNA/48-clustalw.dnd \
		\
		examples/Sequences/Feng-Doolittle-1987/globins.fasta \
		examples/Sequences/Feng-Doolittle-1987/globins-muscle.fasta \
		examples/Sequences/Feng-Doolittle-1987/globins-mafft.fasta \
		examples/Sequences/Feng-Doolittle-1987/globins-fsa.fasta \
		examples/Sequences/Feng-Doolittle-1987/globins-prank.1.fasta \
		examples/Sequences/Feng-Doolittle-1987/globins-prank.1.dnd \
		examples/Sequences/Feng-Doolittle-1987/globins-prank.2.fasta \
		examples/Sequences/Feng-Doolittle-1987/globins-prank.2.dnd \
		examples/Sequences/Feng-Doolittle-1987/globins-bali-phy-C20-gamma4.fasta \
		examples/Sequences/Feng-Doolittle-1987/globins-bali-phy-C20-gamma4-c50.PP.tree \
		examples/Sequences/Feng-Doolittle-1987/globins-bali-phy-C20-gamma4.MAP.tree \
		examples/Sequences/Feng-Doolittle-1987/sod.fasta \
		examples/Sequences/Feng-Doolittle-1987/sod-muscle.fasta \
		examples/Sequences/Feng-Doolittle-1987/sod-mafft.fasta \
		examples/Sequences/Feng-Doolittle-1987/sod-fsa.fasta \
		examples/Sequences/Feng-Doolittle-1987/sod-prank.1.fasta \
		examples/Sequences/Feng-Doolittle-1987/sod-prank.1.dnd \
		examples/Sequences/Feng-Doolittle-1987/sod-prank.2.fasta \
		examples/Sequences/Feng-Doolittle-1987/sod-prank.2.dnd \
		examples/Sequences/Feng-Doolittle-1987/sod-bali-phy-C20-gamma4.fasta \
		examples/Sequences/Feng-Doolittle-1987/sod-bali-phy-C20-gamma4-c50.PP.tree \
		examples/Sequences/Feng-Doolittle-1987/sod-bali-phy-C20-gamma4.MAP.tree \
		examples/Sequences/Feng-Doolittle-1987/v-src.fasta \
		examples/Sequences/Feng-Doolittle-1987/v-src-muscle.fasta \
		examples/Sequences/Feng-Doolittle-1987/v-src-mafft.fasta \
		examples/Sequences/Feng-Doolittle-1987/v-src-fsa.fasta \
		\
		examples/Sequences/Globins/many.fasta \
		examples/Sequences/Globins/few.fasta \
		examples/Sequences/Globins/few-clustalw.fasta \
		examples/Sequences/Globins/few-clustalw.dnd \
		examples/Sequences/Globins/few-muscle.fasta \
		examples/Sequences/Globins/bglobin.fasta \
		\
		examples/Sequences/Miklos-2008/annexin.fasta \
		examples/Sequences/Miklos-2008/haloperoxidase.fasta \
		examples/Sequences/Miklos-2008/lectin.fasta \
		examples/Sequences/Miklos-2008/legume_lectin.fasta \
		examples/Sequences/Miklos-2008/lysozyme.fasta \
		examples/Sequences/Miklos-2008/papain.fasta \
		examples/Sequences/Miklos-2008/parvalbumin-like.fasta \
		examples/Sequences/Miklos-2008/rr_receiver.fasta \
		examples/Sequences/Miklos-2008/Src_homology_2_domains.fasta \
		examples/Sequences/Miklos-2008/Starch_binding_domain.fasta \
		examples/Sequences/Miklos-2008/xylose_isomerase.fasta \
		\
		examples/Sequences/Enolase/enolase-38.fasta \
		examples/Sequences/Enolase/enolase-38-trimmed.fasta \
		\
		examples/Sequences/HIV/chain-2005/env-clustal-codons.fasta \
		examples/Sequences/HIV/chain-2005/env-clustal.fasta \
		examples/Sequences/HIV/chain-2005/env.fasta \
		examples/Sequences/HIV/chain-2005/pol-clustal.fasta \
		examples/Sequences/HIV/chain-2005/pol.fasta \
		examples/Models/Airline.hs \
		examples/Models/CoalMine.hs \
		examples/Models/Demo.hs \
		examples/Models/Line.hs examples/Models/xs examples/Models/ys


EXTRA_HEADERS = @top_srcdir@/include/Eigen

EXTRA_DIST = $(doc_DATA) $(EXTRA_HEADERS) $(bin_SCRIPTS) $(noinst_SCRIPTS) \
             $(nobase_doc_SCRIPTS) $(nobase_examples_DATA) doc/Doxyfile $(nobase_modules_DATA)

dist-hook:
	rm -rf `find $(distdir) -name TODO`
