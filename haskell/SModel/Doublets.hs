module SModel.Doublets where

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Nucleotides

builtin f2x4_frequencies_builtin 3 "SModel:f2x4_frequencies"
builtin singlet_to_doublet_rates 3 "SModel:singlet_to_doublet_rates"

x2x2 a (ReversibleMarkov _ _ q_1 pi_1 _ _ _) (ReversibleMarkov _ _ q_2 pi_2 _ _ _) =
    let smap = simple_smap a
        q = singlet_to_doublet_rates a q_1 q_2
        pi = f2x4_frequencies_builtin a pi_1 pi_2
    in reversible_markov a smap q pi

x2_sym a s = singlet_to_doublet_rates a s s
x2 a q = x2x2 a q q

builtin rna_stem_16a_exchange 6 "SModel:rna_16a_exchange"

rna_stem_16a a aS aD b g e pi = gtr a (rna_stem_16a_exchange a aS aD b g e) pi

builtin rna_editting_rates 3 "SModel:rna_editting_rates"
builtin rna_editting_pi 3 "SModel:rna_editting_pi"

rna_editting a (ReversibleMarkov _ _ q_nuc pi_nuc _ _ _) edits = reversible_markov a smap q pi 
    where smap = simple_smap a
          nuc_a = getNucleotides a
          q = rna_editting_rates a q_nuc edits'
          pi = rna_editting_pi a pi_nuc edits'
          edits' = list_to_vector [ c_pair (find_letter nuc_a i) (find_letter nuc_a j) | (i,j) <- edits]
