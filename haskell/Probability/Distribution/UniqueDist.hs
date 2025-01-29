module Probability.Distribution.UniqueDist where

import Probability.Random
import MCMC
import Data.Unique

data UniqueDist = UniqueDist

instance Dist UniqueDist where
    type Result UniqueDist = Unique
    dist_name _ = "unique"

instance IOSampleable UniqueDist where
    sampleIO UniqueDist = newUnique

-- Hmm.. Maybe we want the result of discrete distributionts to be Probability instead of LogDouble?

instance HasPdf UniqueDist where
    pdf UniqueDist u = 1

instance HasAnnotatedPdf UniqueDist where
    annotated_densities UniqueDist u = return ([],())

instance Sampleable UniqueDist where
    sample UniqueDist = RanDistribution2 UniqueDist do_nothing


uniqueDist = UniqueDist
uniqueInt = hashUnique <$> sample uniqueDist
