module SModel.Nucleotides (module SModel.Nucleotides) where

import SModel.ReversibleMarkov

tn93_sym a k1 k2 = gtr_sym [1.0, k1, 1.0, 1.0, k2, 1.0] a
hky85_sym a k = tn93_sym a k k

k80 a kappa     = gtr a (hky85_sym a kappa) (uniform_frequencies a)
hky85 a k    pi = gtr a (hky85_sym a k) pi
tn93 a k1 k2 pi = gtr a (tn93_sym a k1 k2) pi

