module SModel.Codons (module SModel.Codons) where 
{
import Alphabet;
import SModel.ReversibleMarkov;
import SModel.Nucleotides;

builtin m0 3 "m0" "SModel";
builtin f2x4_frequencies_builtin 3 "f2x4_frequencies" "SModel";
builtin f3x4_frequencies_builtin 4 "f3x4_frequencies" "SModel";
builtin singlet_to_doublet_rates 3 "singlet_to_doublet_rates" "SModel";
builtin singlet_to_triplet_rates 4 "singlet_to_triplet_rates" "SModel";
builtin fMutSel_q 4 "fMutSel_q" "SModel";
builtin fMutSel_pi 3 "fMutSel_pi" "SModel";
builtin dNdS_matrix 2 "dNdS_matrix" "SModel";

f3x4_frequencies a pi1 pi2 pi3 = let {pi1' = list_to_vector pi1;
                                      pi2' = list_to_vector pi2;
                                      pi3' = list_to_vector pi3}
                                  in list_from_vector $ f3x4_frequencies_builtin a pi1' pi2' pi3';

f3x4'_frequencies a pi1 pi2 pi3 = zip (alphabet_letters a) (f3x4_frequencies a pi1' pi2' pi3')
    where {pi1' = get_ordered_elements nuc_letters pi1 "frequencies";
           pi2' = get_ordered_elements nuc_letters pi2 "frequencies";
           pi3' = get_ordered_elements nuc_letters pi3 "frequencies";
           nuc_letters = alphabet_letters a_nuc;
           a_nuc = getNucleotides a};

f1x4_frequencies a pi = f3x4_frequencies a pi pi pi;

f1x4'_frequencies a pi = f3x4'_frequencies a pi pi pi;

gy94_ext  sym w pi a = gtr a (m0 a sym w) pi;

gy94  k w pi a = gy94_ext  sym w pi a where {sym = hky85_sym k $ getNucleotides a};

mg94_ext a q w = dNdS (x3 q a) w;
mg94k a k pi w  = mg94_ext a (hky85 k pi nuc_a) w where {nuc_a = getNucleotides a};
mg94  a   pi w  = mg94_ext a (f81     pi nuc_a) w where {nuc_a = getNucleotides a};

x3x3 (ReversibleMarkov _ _ q_1 pi_1 _ _ _) (ReversibleMarkov _ _ q_2 pi_2 _ _ _) (ReversibleMarkov _ _ q_3 pi_3 _ _ _) a =
    let {smap = simple_smap a;
         q = singlet_to_triplet_rates a q_1 q_2 q_3;
         pi = f3x4_frequencies_builtin a pi_1 pi_2 pi_3}
    in reversible_markov a smap q pi;

x3_sym a s = singlet_to_triplet_rates a s s s;
x3 q a = x3x3 q q q a;

x2x2 (ReversibleMarkov _ _ q_1 pi_1 _ _ _) (ReversibleMarkov _ _ q_2 pi_2 _ _ _) a =
    let {smap = simple_smap a;
         q = singlet_to_doublet_rates a q_1 q_2;
         pi = f2x4_frequencies_builtin a pi_1 pi_2}
    in reversible_markov a smap q pi;

x2_sym a s = singlet_to_doublet_rates a s s;
x2 q a = x2x2 q q a;

-- maybe this should be t*(q %*% dNdS_matrix) in order to avoid losing scaling factors?  Probably this doesn't matter at the moment.
dNdS (ReversibleMarkov a s q pi l t r) omega = reversible_markov a s q2 pi where {q2 = q %*% dNdS_matrix a omega};
}
