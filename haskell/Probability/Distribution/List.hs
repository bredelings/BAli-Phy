module Probability.Distribution.List where

import Probability.Random
import Probability.Distribution.Gamma
import Data.Array
import Effect
import MCMC
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

independent_densities (d:ds) (x:xs) = densities d x ++ independent_densities ds xs
independent_densities [] []         = []
independent_densities _  _          = [doubleToLogDouble 0.0]

independent_pdf (d:ds) (x:xs) = pdf d x * independent_pdf ds xs
independent_pdf [] []         = 1
independent_pdf _  _          = 0

plate n dist_f = independent $ map dist_f [0..n-1]

{-
 Can we generalize this to something that works for IntMap a as well as List a?

type Range :: Type -> Type
type family Range m
type instance Range [] = Int
type instance Range IntMap = IntSet

data IID m d = IID (Range m) d

instance Dist d => dist (IID m d) where
    type Result (IID m d) = m (Result d)

instance IOSampleable d => (IOSampleable (IID m d)) where
   sampleIO (IID range dist) = fmap (

instance IOSampleable d => (IOSampleable (IID [] d)) where
   sampleIO (IID Int dist) = take n <$> (sequence $ repeat $ sampleIO dist)
   sampleIO (IID Int dist) = sequence $ take n $ repeat $ sampleIO dist)

instance IOSampleable d => (IOSampleable (IID IntMap d)) where
   sampleIO (IID rangeSet dist) = sequence $ fromSet (\d -> sampleIO dist) range
-}




-- Its possible to construct something like IID n 
data IID d = IID Int d

instance Dist d => Dist (IID d) where
    type Result (IID d) = [Result d]
    dist_name (IID _ d) = "iid " ++ dist_name d

instance IOSampleable d => (IOSampleable (IID d)) where
    sampleIO (IID n dist) = take n <$> (sequence $ repeat $ sampleIO dist)

instance HasPdf d => HasPdf (IID d) where
    pdf (IID n dist) xs | length xs /= n  = 0
                        | otherwise       = product (map (pdf dist) xs)

instance HasAnnotatedPdf d => HasAnnotatedPdf (IID d) where
    annotated_densities (IID n dist) = make_densities' $ independent_densities (replicate n dist)

instance Sampleable d => Sampleable (IID d) where
    sample (IID n dist) = lazy $ RanSamplingRate (1/sqrt (fromIntegral n)) $ do
                                    dist <- interchangeable $ sample dist
                                    xs <- sequence $ repeat $ dist
                                    return $ take n xs

iid n dist = IID n dist

-- OK, so part of the issue here is that we want iid n sampling_action to work,
-- but we want iid_dist n dist to require dist to be something with a pdf.
-- So... we want only (iid n (normal 0 1)) and iidDist n (normalDist 0 1), I guess?
-- ... and then iidDist n is sampleable only if the dist is also sampleable?
-- that DOES make sense...

-- So we can do this...  but then we only get access to the "dist" part.
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

-----
data IIDOn a d = IIDOn [a] d

instance Dist d => Dist (IIDOn a d) where
    type Result (IIDOn a d) = [(a, Result d)]

instance Sampleable d => Sampleable (IIDOn a d) where
    sample (IIDOn vs dist) = let n = length vs
                             in lazy $ RanSamplingRate (1/sqrt (fromIntegral n)) $ do
                                  dist <- interchangeable $ sample dist
                                  xs <- sequence $ repeat $ dist
                                  return $ zip vs xs

iid_on items dist = IIDOn items dist

{-
  could we do i.e.
  strict $ do
    sampling_rate (1/sqrt (fromIntegral n)
    xs <- lazy $ sequence $ repeat dist  -- lazy means no effects (add_move) or rate changes (sample_rate)
    add_move $ interchange_list_entries xs  -- ideally this is only allowed within strict blocks!  And MCMC-specific blocks?
    return $ take n xs
-}


iid2 n dist1 dist2 = sample $ iid n $ (,) <$> sample dist1 <*> sample dist2

iid_mixture_dist n weight_dist item_dist = do
    (ps, items) <- unzip <$> iid2 n weight_dist item_dist
    return (map (/sum ps) ps, items)

dirichlet_mixture_dist n a item_dist = iid_mixture_dist n (gamma a 1) item_dist


even_sorted_on_iid f n dist = do let n_all = 2*n+1
                                 xs' <- sample $ iid n_all dist
                                 let xs = listArray n_all $ sortOn f xs'
                                 return [xs!(2*i-1) | i <- [1..n]]

even_sorted_iid = even_sorted_on_iid id


odd_sorted_on_iid f n dist = do let n_all = max (2*n-1) 0
                                xs' <- sample $ iid n_all dist
                                let xs = listArray n_all $ sortOn f xs'
                                return [xs!(2*i) | i <- [0..n-1]]

odd_sorted_iid = odd_sorted_on_iid id
{-
odd_iid_mixture_dist n weight_dist item_dist = do
    (ps, items) <- unzip <$> odd_sorted_on_iid snd n $ (,) <$> weight_dist <*> item_dist
    return (map (/sum ps) ps, items)

even_iid_mixture_dist n weight_dist item_dist = do
    (ps, items) <- unzip <$> even_sorted_on_iid snd n $ (,) <$> weight_dist <*> item_dist
    return (map (/sum ps) ps, items)

dirichlet_odd_mixture_dist  n a item_dist = odd_iid_mixture_dist  n (gamma a 1) item_dist
dirichlet_even_mixture_dist n a item_dist = even_iid_mixture_dist n (gamma a 1) item_dist
-}

{-
  So, the need to couple the two distributions is very inconvenient!
  It would be much easier if we could generate the dirichlet separately,
  ... but that is inherently using the index of the weight and the item to
  decide which items and weights correspond!
-}
