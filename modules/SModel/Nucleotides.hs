module SModel.Nucleotides (module SModel.Nucleotides) where
{
  import SModel.ReversibleMarkov;

  tn93_sym k1 k2 a = gtr_sym [1.0, k1, 1.0, 1.0, k2, 1.0] a;
  hky85_sym k a = tn93_sym k k a;

  k80 kappa nuca = reversible_markov (hky85_sym kappa nuca) (plus_f_equal_frequencies nuca);
  hky85 k    pi a = reversible_markov (hky85_sym k    a) (plus_f a pi);
  tn93 k1 k2 pi a = reversible_markov (tn93_sym k1 k2 a) (plus_f a pi);

  hky85' k    pi' a = reversible_markov (hky85_sym k    a) (plus_f' a pi');
  tn93' k1 k2 pi' a = reversible_markov (tn93_sym k1 k2 a) (plus_f' a pi');
}
