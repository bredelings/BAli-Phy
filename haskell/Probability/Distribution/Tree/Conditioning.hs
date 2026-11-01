module Probability.Distribution.Tree.Conditioning where

-- Crown denotes the observed crown: both initial lineages have sampled descendants.
data Start = Origin | Crown

data AgeTreatment = AtAge Double | Flat

-- These retain their birth-death meanings even when survival is certain under Yule.
data Condition = Unconditioned | Surviving | Tips Int
