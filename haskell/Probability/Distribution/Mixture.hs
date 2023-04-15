module Probability.Distribution.Mixture where

import Probability.Random
import Probability.Distribution.Categorical
import Probability.Distribution.Discrete

{-
   A (Mixture d) is an example of a "d" where we perform the d by
    1. Randomly drawing the d using either IO or Random
    2. Perform the d.

   In cases where x is a distribution, Mixture d is also a distribution.

   Hmm.. should I parameterize Sampleable over the monad where its sampleable?
   Would that remove the redundancy?
-}

data Mixture d = Mixture d

instance (Dist d, Dist (Result d)) => Dist (Mixture d) where
    type Result (Mixture d) = Result (Result d)
    dist_name _ = "mixture"

instance (IOSampleable d, IOSampleable (Result d)) => IOSampleable (Mixture d) where
    sampleIO (Mixture dist1) = do dist2 <- sampleIO dist1
                                  sampleIO dist2

instance (Sampleable d, Sampleable (Result d)) => Sampleable (Mixture d) where
    sample (Mixture dist1) = do dist2 <- sample dist1
                                sample dist2

instance HasPdf d => HasPdf (Mixture (Discrete d)) where
    pdf (Mixture (Discrete pairs)) x = sum [ doubleToLogDouble p * pdf dist x | (dist,p) <- pairs ]

instance HasAnnotatedPdf d => HasAnnotatedPdf (Mixture (Discrete d)) where
    annotated_densities (Mixture (Discrete pairs)) x = return $ [sum [doubleToLogDouble p * density dist x | (dist,p) <- pairs]]


mixtureDist ps dists | length ps /= length dists  = error "mixture distribution has different number of weights and distributions"
                     | otherwise                  = Mixture $ Discrete $ zip dists ps 

mixture ps dists = sample $ mixtureDist ps dists

----------------------------------------------------------



-- In cases where x is a distribution, Mixture d is also a distribution.
-- But how about when x is something else?  Like an action?  Then shouldn't Mixture d be an action that can be taken?
-- For example, suppose we have a Mixture (IO a).  Then shouldn't this ALSO be an IO a?
--
-- Perhaps Mixture should then be a functor. Perhaps Mixture d a should return IO (d a) or Random (d a)
-- Should we also attempt to join then?  Random (Random a) could reduce to Random a.
-- But how about IO (Random a)
-- Random (IO a) should be performable in IO as join (sample IO)
