module Probability.Distribution.Independent where

import Probability.Random
import qualified Data.IntMap as IntMap

{-NOTE: It seems like sampling from an Independent f d should turn into a
        collection of samples.

The `independent` distribution relies on Functor f to go from
     f (Random a) -> Random (f a)
in a lazy fashion.

There is no downweighting of the MCMC sampling for larger objects.
If we could count the number of forced entries that might work,
 although we'd have to we aware of how this differs in the initial and proposed point.
-}

{- NOTE: It seems like observing from an Independent f d should turn into a
         collection of observes.

I guess we are doing observes through annotated_densities.
It calls densities on the components, and densities calls
annotated_densities on the component distributions.
-}

{- QUESTION: Is there a way to observe some of the elements while sampling other?

-}

independent_densities (d:ds) (x:xs) = densities d x ++ independent_densities ds xs
independent_densities [] []         = []
independent_densities _  _          = [doubleToLogDouble 0.0]

independent_pdf (d:ds) (x:xs) = pdf d x * independent_pdf ds xs
independent_pdf [] []         = 1
independent_pdf _  _          = 0

plate n dist_f = independent $ map dist_f [0..n-1]

data Independent f d = Independent (f d)

instance Dist d => Dist (Independent f d) where
    type Result (Independent f d) = f (Result d)
    dist_name dist = "independent"

instance (Functor f, IOSampleable d) => IOSampleable (Independent f d) where
    sampleIO (Independent dists) = return $ fmap (unsafePerformIO . sampleIO) dists

instance HasPdf d => HasPdf (Independent [] d) where
    pdf (Independent ds) xs = independent_pdf ds xs

instance HasAnnotatedPdf d => HasAnnotatedPdf (Independent [] d) where
    annotated_densities (Independent dists) = make_densities' $ independent_densities dists

--instance Sampleable d => Sampleable (Independent [] d) where
--    sample (Independent dists) = lazy $ sequence $ map sample dists

--instance Sampleable d => Sampleable (Independent IntMap d) where
--    sample (Independent dists) = RanOp (\interp -> return $ fmap (unsafePerformIO . interp . sample) dists)

instance (Functor f, Sampleable d) => Sampleable (Independent f d) where
    sample (Independent dists) = RanOp (\interp -> return $ fmap (unsafePerformIO . interp . sample) dists)

independent dists = Independent dists

independentMap set func = independent $ (set & IntMap.fromSet func)
