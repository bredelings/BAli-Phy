module SModel.Codons (module SModel.Codons) where 

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Nucleotides

builtin m0 3 "m0" "SModel"
builtin f3x4_frequencies_builtin 4 "f3x4_frequencies" "SModel"
builtin singlet_to_triplet_rates 4 "singlet_to_triplet_rates" "SModel"
builtin dNdS_matrix 2 "dNdS_matrix" "SModel"

f3x4_frequencies a pi1 pi2 pi3 = let pi1' = list_to_vector pi1
                                     pi2' = list_to_vector pi2
                                     pi3' = list_to_vector pi3
                                  in list_from_vector $ f3x4_frequencies_builtin a pi1' pi2' pi3'

f3x4'_frequencies a pi1 pi2 pi3 = zip (letters a) (f3x4_frequencies a pi1' pi2' pi3')
    where pi1' = get_ordered_elements nuc_letters pi1 "frequencies"
          pi2' = get_ordered_elements nuc_letters pi2 "frequencies"
          pi3' = get_ordered_elements nuc_letters pi3 "frequencies"
          nuc_letters = letters a_nuc
          a_nuc = getNucleotides a

f1x4_frequencies a pi = f3x4_frequencies a pi pi pi

f1x4'_frequencies a pi = f3x4'_frequencies a pi pi pi

gy94_ext  sym w pi a = gtr a (m0 a sym w) pi

gy94  k w pi a = gy94_ext  sym w pi a where sym = hky85_sym (getNucleotides a) k

mg94_ext a w q = q & x3 a & dNdS w
mg94k a k pi w  = hky85 nuc_a k pi & mg94_ext a w where nuc_a = getNucleotides a
mg94  a   pi w  = f81     pi nuc_a & mg94_ext a w where nuc_a = getNucleotides a

x3x3 a (ReversibleMarkov _ _ q_1 pi_1 _ _ _) (ReversibleMarkov _ _ q_2 pi_2 _ _ _) (ReversibleMarkov _ _ q_3 pi_3 _ _ _) =
    let smap = simple_smap a
        q = singlet_to_triplet_rates a q_1 q_2 q_3
        pi = f3x4_frequencies_builtin a pi_1 pi_2 pi_3
    in reversible_markov a smap q pi

x3_sym a s = singlet_to_triplet_rates a s s s
x3 a q = x3x3 a q q q

-- maybe this should be t*(q %*% dNdS_matrix) in order to avoid losing scaling factors?  Probably this doesn't matter at the moment.
dNdS omega (ReversibleMarkov a s q pi l t r) = reversible_markov a s q2 pi where q2 = q %*% dNdS_matrix a omega
