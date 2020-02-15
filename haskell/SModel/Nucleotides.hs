module SModel.Nucleotides (module SModel.Nucleotides) where

import SModel.ReversibleMarkov;

tn93_sym k1 k2 a = gtr_sym [1.0, k1, 1.0, 1.0, k2, 1.0] a
hky85_sym k a = tn93_sym k k a

k80 kappa a     = gtr a (hky85_sym kappa a) (uniform_frequencies a)
hky85 k    pi a = gtr a (hky85_sym k a) pi
tn93 k1 k2 pi a = gtr a (tn93_sym k1 k2 a) pi

