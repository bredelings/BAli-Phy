module SModel.Nucleotides (module SModel.Nucleotides) where

import SModel.ReversibleMarkov

tn93_sym a k1 k2 = gtr_sym [1.0, k1, 1.0, 1.0, k2, 1.0] a
hky85_sym a k = tn93_sym a k k

k80 a kappa     = gtr (hky85_sym a kappa) (uniform_frequencies a)
hky85 a k    pi = gtr (hky85_sym a k) pi
tn93 a k1 k2 pi = gtr (tn93_sym a k1 k2) pi

hky85' a k pi = hky85 a k pi' where pi' = frequencies_from_dict a pi
tn93' a k1 k2 pi = tn93 a k1 k2 pi' where pi' = frequencies_from_dict a pi
