module Probability.Distribution.ATModelDist where

import Probability.Random
import BAliPhy.ATModel
import qualified BAliPhy.ATModel.DataPartition as DP -- for `likelihood`

atmodel_likelihood atm = map DP.likelihood (partitions atm)
atmodel_sample_error = error "sampling from atmodel_dist is not allowed.  You can only observe it."
atmodel_range_error = error "atmodel_dist has no range.  You can only observe it."
atmodel_dist = Distribution (\x-> atmodel_likelihood x) (no_quantile "atmodel_dist") atmodel_sample_error atmodel_range_error
