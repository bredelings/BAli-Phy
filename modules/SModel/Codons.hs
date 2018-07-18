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

  gy94  k w pi  a = reversible_markov (m0 a (hky85_sym k a_nuc) w) (plus_f  a pi ) where {a_nuc = getNucleotides a};
  gy94' k w pi' a = reversible_markov (m0 a (hky85_sym k a_nuc) w) (plus_f' a pi') where {a_nuc = getNucleotides a};
}
