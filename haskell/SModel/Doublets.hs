module SModel.Doublets where

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Nucleotides

builtin f2x4_frequencies_builtin 3 "f2x4_frequencies" "SModel"
builtin singlet_to_doublet_rates 3 "singlet_to_doublet_rates" "SModel"

x2x2 a (ReversibleMarkov _ _ q_1 pi_1 _ _ _) (ReversibleMarkov _ _ q_2 pi_2 _ _ _) =
    let smap = simple_smap a
        q = singlet_to_doublet_rates a q_1 q_2
        pi = f2x4_frequencies_builtin a pi_1 pi_2
    in reversible_markov a smap q pi

x2_sym a s = singlet_to_doublet_rates a s s
x2 a q = x2x2 a q q

builtin rna_stem_16a_exchange 6 "rna_16a_exchange" "SModel"

rna_stem_16a a aS aD b g e pi = gtr a (rna_stem_16a_exchange a aS aD b g e) pi

