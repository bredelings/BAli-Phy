# Myxovirus resistance (Mx) genes

This example contains aligned protein-coding sequences of myxovirus resistance (Mx) genes
from ten mammal and two bird species. These are vertebrate genes involved in antiviral
defence, not sequences from myxoviruses.

The data were taken from the tutorial accompanying:

Álvarez-Carretero S, Kapli P, Yang Z (2023). *Beginner's Guide on the Use of PAML to Detect
Positive Selection*. Molecular Biology and Evolution 40(4): msad041.
<https://doi.org/10.1093/molbev/msad041>

The data set is redistributed with permission from the lead author, Sandra Álvarez-Carretero.

The tutorial authors downloaded sequences based on the accession list for data set 1 in
Hou et al. (2007) and generated a new alignment, because the earlier study did not provide
its alignment. Their [data preparation notes](https://github.com/abacus-gene/paml-tutorial/tree/main/positive-selection/00_data)
describe the downloads, accession corrections, and alignment procedure. This is the alignment
prepared for the 2023 tutorial, not the unavailable original alignment from Hou et al.

## Files

- `Mx_aln.phy`: original PHYLIP alignment, copied unchanged from the Selection project.
  It contains 12 sequences and 1,992 nucleotide columns, including terminal stop codons.
- `Mx_aln.nonstop.fasta`: the same alignment in FASTA format, with nucleotide columns
  1990–1992 removed. These contain the terminal stop codon in every sequence. Names, order,
  and all remaining bases and gaps are preserved. The result has 1,989 columns (663 codons),
  no internal in-frame stop codons under the standard genetic code, and whole-codon gaps.
- `Mx_foreground.tree`: the unrooted topology supplied with the PAML tutorial, in plain
  Newick format, with BAli-Phy branch annotations from the Selection project. The terminal
  chicken and duck branches have `:[&foreground=1]`; all other branches are unlabelled.
  Use this tree with `BranchModel` or `BranchSite`.
- `Mx_branch_test.tree`: the same topology, with `:[&foreground={0,1}]` on both terminal
  bird branches. Use this tree with `BranchModel_test`.

In the branch test, hypothesis 0 assigns every branch to category 0. Hypothesis 1 assigns
the chicken and duck terminal branches to a shared category 1 and leaves every other branch
in category 0. The ancestral branch leading to birds is not part of the foreground group.
Annotations follow a colon because they describe branches, not nodes. Fixing this topology
in BAli-Phy still allows branch lengths to be estimated.

The BAli-Phy tutorial first conditions on the supplied alignment with `-I none`, then shows
how to estimate the alignment jointly with selection by removing that option. Its models
and Bayesian tests differ from the PAML analyses; these examples do not reproduce the
paper's numerical results.
