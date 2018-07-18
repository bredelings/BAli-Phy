module SModel.Codons (module SModel.Codons) where 
{
import Alphabet;
import SModel.ReversibleMarkov;
import SModel.Nucleotides;

builtin m0 3 "m0" "SModel";
builtin f3x4_frequencies_builtin 4 "f3x4_frequencies" "SModel";
builtin muse_gaut_matrix 4 "muse_gaut_matrix" "SModel";
builtin singlet_to_triplet_exchange 2 "singlet_to_triplet_exchange" "SModel";
builtin fMutSel_q 4 "fMutSel_q" "SModel";
builtin fMutSel_pi 3 "fMutSel_pi" "SModel";

f3x4_frequencies a pi1 pi2 pi3 = let {pi1' = list_to_vector pi1;
                                      pi2' = list_to_vector pi2;
                                      pi3' = list_to_vector pi3}
                                  in list_from_vector $ f3x4_frequencies_builtin a pi1' pi2' pi3';

f3x4_frequencies_vec a pi1 pi2 pi3 = list_to_vector $ f3x4_frequencies a pi1 pi2 pi3;

f3x4'_frequencies a pi1 pi2 pi3 = zip (alphabet_letters a) (f3x4_frequencies a pi1' pi2' pi3')
    where {pi1' = get_ordered_elements nuc_letters pi1 "frequencies";
           pi2' = get_ordered_elements nuc_letters pi2 "frequencies";
           pi3' = get_ordered_elements nuc_letters pi3 "frequencies";
           nuc_letters = alphabet_letters a_nuc;
           a_nuc = getNucleotides a};

f1x4_frequencies a pi = f3x4_frequencies a pi pi pi;

f1x4_frequencies_vec a pi = f3x4_frequencies_vec a pi pi pi;

f1x4'_frequencies a pi = f3x4'_frequencies a pi pi pi;

mg94_freq nuc_pi triplet_a = let {nuc_a          = getNucleotides triplet_a;
                             triplet_pi_vec = f1x4_frequencies_vec triplet_a nuc_pi;
                             nuc_pi_vec     = list_to_vector nuc_pi;
                             nuc_r          = plus_f_matrix nuc_a nuc_pi_vec} in
                        ReversibleFrequency triplet_a (simple_smap triplet_a) triplet_pi_vec (muse_gaut_matrix triplet_a nuc_r nuc_r nuc_r);

mg94w9_freq nuc_pi1 nuc_pi2 nuc_pi3 triplet_a = let {nuc_a          = getNucleotides triplet_a;
                                                triplet_pi_vec = f3x4_frequencies_vec triplet_a nuc_pi1 nuc_pi2 nuc_pi3;
                                                nuc_pi_vec1     = list_to_vector nuc_pi1;
                                                nuc_pi_vec2     = list_to_vector nuc_pi2;
                                                nuc_pi_vec3     = list_to_vector nuc_pi3;
                                                nuc_r1         = plus_f_matrix nuc_a nuc_pi_vec1;
                                                nuc_r2         = plus_f_matrix nuc_a nuc_pi_vec2;
                                                nuc_r3         = plus_f_matrix nuc_a nuc_pi_vec3} in
                                           ReversibleFrequency triplet_a (simple_smap triplet_a) triplet_pi_vec (muse_gaut_matrix triplet_a nuc_r1 nuc_r2 nuc_r3);

mg94'_freq pi' a = mg94_freq pi a  where {nuc_letters = alphabet_letters (getNucleotides a);
                                          pi = get_ordered_elements nuc_letters pi' "frequencies"};

mg94w9'_freq pi1' pi2' pi3' a = mg94w9_freq pi1 pi2 pi3 a where {nuc_letters = alphabet_letters (getNucleotides a);
                                                       pi1 = get_ordered_elements nuc_letters pi1' "frequencies";
                                                       pi2 = get_ordered_elements nuc_letters pi2' "frequencies";
                                                       pi3 = get_ordered_elements nuc_letters pi3' "frequencies"};

gy94  k w pi a = reversible_markov (m0 a (hky85_sym k a_nuc) w) (plus_f  a pi) where {a_nuc = getNucleotides a};
gy94' k w pi a = reversible_markov (m0 a (hky85_sym k a_nuc) w) (plus_f' a pi) where {a_nuc = getNucleotides a};

mg94  k w pi a = reversible_markov (m0 a (hky85_sym k a_nuc) w) (mg94_freq  pi a) where {a_nuc = getNucleotides a};
mg94' k w pi a = reversible_markov (m0 a (hky85_sym k a_nuc) w) (mg94'_freq pi a) where {a_nuc = getNucleotides a};

mg94w9  k w pi1 pi2 pi3 a = reversible_markov (m0 a (hky85_sym k a_nuc) w) (mg94w9_freq  pi1 pi2 pi3 a) where {a_nuc = getNucleotides a};
mg94w9' k w pi1 pi2 pi3 a = reversible_markov (m0 a (hky85_sym k a_nuc) w) (mg94w9'_freq pi1 pi2 pi3 a) where {a_nuc = getNucleotides a};

q3 (ReversibleMarkov _ _ q_1 pi_1 _ _ _) (ReversibleMarkov _ _ q_2 pi_2 _ _ _) (ReversibleMarkov _ _ q_3 pi_3 _ _ _) a =
    let {nuc_a = getNucleotides a;
         smap = simple_smap a;
         q = muse_gaut_matrix a q_1 q_2 q_3;
         pi = f3x4_frequencies_builtin a pi_1 pi_2 pi_3}
    in reversible_markov' a smap q pi;
}
