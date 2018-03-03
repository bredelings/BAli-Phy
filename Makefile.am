# Help aclocal find the pkg.m4 file in the m4/ directory
ACLOCAL_AMFLAGS = -I m4

# Should we build our own copy of some BOOST libraries?
if NO_SYSTEM_BOOST
  MAYBE_BOOST = external/boost/
endif

# Build in these subdirectories
SUBDIRS = $(MAYBE_BOOST) src/ 

# Install these scripts in bin/
bin_SCRIPTS = scripts/bali-phy-sge scripts/bp-analyze scripts/bali-phy-pkg

# Install these scripts in libexec
scriptsdir = $(libdir)/@PACKAGE@/libexec
scripts_DATA = scripts/compare-runs.R scripts/compare-runs2.R \
	       scripts/tree-plot1.R scripts/tree-plot2.R scripts/tree-plot3.R \
	       scripts/tree-plot2-3D.R scripts/tree-plot1-3D.R scripts/tree-plot3-3D.R

# Used to build, but not distributed
noinst_SCRIPTS = scripts/git_version.sh

modulesdir = $(libdir)/@PACKAGE@/
nobase_modules_DATA = modules/Distributions.hs modules/PopGen.hs modules/Prelude.hs modules/Range.hs modules/SModel.hs \
		modules/Tree.hs modules/Alphabet.hs modules/Parameters.hs modules/Alignment.hs \
		modules/MCMC.hs modules/IModel.hs modules/System/Environment.hs modules/Data/ReadFile.hs \
		modules/SubAIndex.hs modules/SMC.hs

helpdir = $(libdir)/@PACKAGE@/
nobase_help_DATA = help/commands/alphabet.txt \
	help/commands/smodel.txt \
	help/commands/imodel.txt \
	help/commands/scale.txt \
	help/commands/branch-lengths.txt \
	help/commands/link.txt \
	help/parameters.txt \
	help/priors.txt \
	help/Triplets.txt \
	help/Codons.txt \
	help/genetic-code.txt \
	help/commands/config.txt \
	help/commands/name.txt \
	help/commands/tree.txt \
	help/commands/version.txt \
	help/commands/test.txt \
	help/commands/verbose.txt \
	help/commands/seed.txt \
	help/commands/iterations.txt \
	help/commands/subsample.txt \
	help/commands/initial-value.txt \
	help/commands/package-path.txt \
	help/commands/align.txt \
	help/commands/pre-burnin.txt \
	help/commands/disable.txt \
	help/commands/enable.txt \
	help/commands/set.txt \
	help/commands/model.txt \
	help/commands/Rao-Blackwellize.txt


functionsdir = $(libdir)/@PACKAGE@/
nobase_functions_DATA = functions/AA.json \
	functions/Add.json \
	functions/Bernoulli.json \
	functions/Beta.json \
	functions/Binomial.json \
	functions/branch-site.json \
	functions/Codons.json \
	functions/Dirichlet.json \
	functions/Div.json \
	functions/DNA.json \
	functions/DP.json \
	functions/dp_omega.json \
	functions/Empirical.json \
	functions/EQU.json \
	functions/exp.json \
	functions/Exponential.json \
	functions/F1x4.json \
	functions/F3x4.json \
	functions/F61.json \
	functions/F81.json \
	functions/F.json \
	functions/fMutSel0.json \
	functions/fMutSel.json \
	functions/Gamma.json \
	functions/Geometric.json \
	functions/GTR.json \
	functions/gwF.json \
	functions/HKY.json \
	functions/iid.json \
	functions/INV.json \
	functions/JC.json \
	functions/JTT.json \
	functions/K80.json \
	functions/Laplace.json \
	functions/length.json \
	functions/LG.json \
	functions/logGamma.json \
	functions/log.json \
	functions/logLaplace.json \
	functions/logNormal.json \
	functions/M0.json \
	functions/M1a.json \
	functions/M2a.json \
	functions/M2a_Test.json \
	functions/M3.json \
	functions/M3_Test.json \
	functions/M7.json \
	functions/M8a.json \
	functions/M8a_Test.json \
	functions/M8.json \
	functions/MG94.json \
	functions/MG94w9.json \
	functions/Mixture.json \
	functions/Mul.json \
	functions/MultiMixtureModel.json \
	functions/MultiRate.json \
	functions/Normal.json \
	functions/Pair.json \
	functions/PAM.json \
	functions/Rates/Gamma.json \
	functions/Rates/logNormal.json \
	functions/RCTMC.json \
	functions/RNA.json \
	functions/RS05.json \
	functions/RS07.json \
	functions/RS07RelaxedRates.json \
	functions/Sample.json \
	functions/Sub.json \
	functions/TN.json \
	functions/UniformInt.json \
	functions/Uniform.json \
	functions/UnitMixture.json \
	functions/WAG.json \
	functions/x3.json \
	functions/DirichletOn.json \
	functions/letters.json \
	functions/getNucleotides.json \
	functions/getAlphabet.json \
	functions/letter_pairs.json \
	functions/intToDouble.json \
	functions/replicate.json \
	functions/Cons.json \
	functions/Nil.json \
	functions/num_branches.json \
	functions/uniform_discretize.json \
	functions/density.json \
	functions/quantile.json \
	functions/take.json \
	functions/zip.json \
	functions/getAminoAcids.json

# Distributed in the doc/ directory
docdir = $(datadir)/doc/@PACKAGE@
doc_DATA = doc/README.html doc/README.xhtml doc/docbook.css doc/README.pdf \
		scripts/plot-path-graph.R



examplesdir = $(datadir)/@PACKAGE@/
nobase_examples_DATA = examples/sequences/EF-Tu/5d.fasta examples/sequences/EF-Tu/12d.fasta examples/sequences/EF-Tu/48b.fasta \
		\
		examples/sequences/5S-rRNA/5d.fasta examples/sequences/5S-rRNA/5d-muscle.fasta \
		examples/sequences/5S-rRNA/5d-clustalw.fasta examples/sequences/5S-rRNA/5d-clustalw.dnd \
                examples/sequences/5S-rRNA/25.fasta examples/sequences/5S-rRNA/25-muscle.fasta \
		examples/sequences/5S-rRNA/25-clustalw.fasta examples/sequences/5S-rRNA/25-clustalw.dnd \
                examples/sequences/5S-rRNA/48.fasta examples/sequences/5S-rRNA/48-muscle.fasta \
		examples/sequences/5S-rRNA/48-clustalw.fasta examples/sequences/5S-rRNA/48-clustalw.dnd \
		\
		examples/sequences/Feng-Doolittle-1987/globins.fasta \
		examples/sequences/Feng-Doolittle-1987/globins-muscle.fasta \
		examples/sequences/Feng-Doolittle-1987/globins-mafft.fasta \
		examples/sequences/Feng-Doolittle-1987/globins-fsa.fasta \
		examples/sequences/Feng-Doolittle-1987/globins-prank.1.fasta \
		examples/sequences/Feng-Doolittle-1987/globins-prank.1.dnd \
		examples/sequences/Feng-Doolittle-1987/globins-prank.2.fasta \
		examples/sequences/Feng-Doolittle-1987/globins-prank.2.dnd \
		examples/sequences/Feng-Doolittle-1987/globins-bali-phy-C20-gamma4.fasta \
		examples/sequences/Feng-Doolittle-1987/globins-bali-phy-C20-gamma4-c50.PP.tree \
		examples/sequences/Feng-Doolittle-1987/globins-bali-phy-C20-gamma4.MAP.tree \
		examples/sequences/Feng-Doolittle-1987/sod.fasta \
		examples/sequences/Feng-Doolittle-1987/sod-muscle.fasta \
		examples/sequences/Feng-Doolittle-1987/sod-mafft.fasta \
		examples/sequences/Feng-Doolittle-1987/sod-fsa.fasta \
		examples/sequences/Feng-Doolittle-1987/sod-prank.1.fasta \
		examples/sequences/Feng-Doolittle-1987/sod-prank.1.dnd \
		examples/sequences/Feng-Doolittle-1987/sod-prank.2.fasta \
		examples/sequences/Feng-Doolittle-1987/sod-prank.2.dnd \
		examples/sequences/Feng-Doolittle-1987/sod-bali-phy-C20-gamma4.fasta \
		examples/sequences/Feng-Doolittle-1987/sod-bali-phy-C20-gamma4-c50.PP.tree \
		examples/sequences/Feng-Doolittle-1987/sod-bali-phy-C20-gamma4.MAP.tree \
		examples/sequences/Feng-Doolittle-1987/v-src.fasta \
		examples/sequences/Feng-Doolittle-1987/v-src-muscle.fasta \
		examples/sequences/Feng-Doolittle-1987/v-src-mafft.fasta \
		examples/sequences/Feng-Doolittle-1987/v-src-fsa.fasta \
		\
		examples/sequences/Globins/many.fasta \
		examples/sequences/Globins/few.fasta \
		examples/sequences/Globins/few-clustalw.fasta \
		examples/sequences/Globins/few-clustalw.dnd \
		examples/sequences/Globins/few-muscle.fasta \
		examples/sequences/Globins/bglobin.fasta \
		\
		examples/sequences/Miklos-2008/annexin.fasta \
		examples/sequences/Miklos-2008/haloperoxidase.fasta \
		examples/sequences/Miklos-2008/lectin.fasta \
		examples/sequences/Miklos-2008/legume_lectin.fasta \
		examples/sequences/Miklos-2008/lysozyme.fasta \
		examples/sequences/Miklos-2008/papain.fasta \
		examples/sequences/Miklos-2008/parvalbumin-like.fasta \
		examples/sequences/Miklos-2008/rr_receiver.fasta \
		examples/sequences/Miklos-2008/Src_homology_2_domains.fasta \
		examples/sequences/Miklos-2008/Starch_binding_domain.fasta \
		examples/sequences/Miklos-2008/xylose_isomerase.fasta \
		\
		examples/sequences/Enolase/enolase-38.fasta \
		examples/sequences/Enolase/enolase-38-trimmed.fasta \
		\
		examples/sequences/HIV/chain-2005/env-clustal-codons.fasta \
		examples/sequences/HIV/chain-2005/env-clustal.fasta \
		examples/sequences/HIV/chain-2005/env.fasta \
		examples/sequences/HIV/chain-2005/pol-clustal.fasta \
		examples/sequences/HIV/chain-2005/pol.fasta \
		examples/models/Airline.hs \
		examples/models/CoalMine.hs \
		examples/models/Demo.hs \
		examples/models/Line.hs examples/models/xs examples/models/ys


EXTRA_HEADERS = @top_srcdir@/include/Eigen

EXTRA_DIST = $(doc_DATA) $(EXTRA_HEADERS) $(bin_SCRIPTS) $(noinst_SCRIPTS) \
             $(nobase_doc_SCRIPTS) $(nobase_examples_DATA) doc/Doxyfile $(nobase_modules_DATA)

dist-hook:
	rm -rf `find $(distdir) -name TODO`

check:
	cd $(abs_top_srcdir)/tests; python $(abs_top_srcdir)/tests/run-tests.py $(abs_top_builddir)/src/bali-phy$(EXEEXT) --package-path=$(abs_top_builddir)/src/builtins/.libs/:$(abs_top_srcdir)/
	cd $(abs_top_srcdir)/testiphy; python $(abs_top_srcdir)/testiphy/testiphy $(abs_top_builddir)/src/bali-phy$(EXEEXT) --package-path=$(abs_top_builddir)/src/builtins/.libs/:$(abs_top_srcdir)/
