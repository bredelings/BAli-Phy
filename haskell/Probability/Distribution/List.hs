module Probability.Distribution.List where

import Probability.Random
import Probability.Distribution.Independent
import Probability.Distribution.Gamma
import Probability.Distribution.Tuple
import Probability.Distribution.Discrete -- Maybe move to a different module?
import Data.Array
import Effect
import MCMC
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

{-
 Can we generalize this to something that works for IntMap a as well as List a?

type Domain :: Type -> Type
type family Domain k
type instance Domain Int     = []
type instance Domain IntSet  = IntMap
type instance Domain (Set a) = Map a  -- Data.Set requires Ord a.
type instance Domain [a]     = [(a,)] -- This can't be applied to b.
                                         So it can't be a Functor.
                                         Would it work to make Domain into a 2-arg type family?

data IID k d = IID k d

instance Dist d => dist (IID m d) where
    type Result (IID k d) = (Domain m) (Result d)

instance IOSampleable d => (IOSampleable (IID m d)) where
   sampleIO (IID keys dist) = sampleIO $ independent ( ??? )

instance IOSampleable d => (IOSampleable (IID [] d)) where
   sampleIO (IID Int dist) = take n <$> (sequence $ repeat $ sampleIO dist)
   sampleIO (IID Int dist) = sequence $ take n $ repeat $ sampleIO dist)

instance IOSampleable d => (IOSampleable (IID IntMap d)) where
   sampleIO (IID rangeSet dist) = sequence $ fromSet (\d -> sampleIO dist) range

For sampling, we need to be able to convert each key set into an (f dist) for some f.

iid (()::())       dist -> repeat dist
   iidInf
iid (n::Int)       dist -> take n (repeat dist) == replicate n dist
   iidN
iid (keys::[k])    dist -> fmap (\key -> (key,) <$> dist) keys
   iidOn
iid (keys::IntSet) dist -> IntMap.fromSet keys (const dist)
   iidOnSet

for computing the probability, then
a) if we can extract a finite list of values `values'`, and if possible then return (map (density dist) values')
b) return zero if we cannot extract the list

iidProb ()                                                 = values' = error "Can't observe an infinite number of values"
iidProb (n::int)       dist (values::[Result dist])        = values' = values if (length values == n)
iidProb (keys::[k])    dist (values::[(k,Result dist)]     = values' = map snd values if (the key sets are the same.)
iidProb (keys::IntSet) dist (values::IntMap (Result dist)) = values' = IntMap.elems values' if (the keysets are the same).
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

iidMap set dist = independent $ IM.fromSet (const dist) set

-- OK, so part of the issue here is that we want iid n sampling_action to work,
-- but we want iid_dist n dist to require dist to be something with a pdf.
-- So... we want only (iid n (normal 0 1)) and iidDist n (normalDist 0 1), I guess?
-- ... and then iidDist n is sampleable only if the dist is also sampleable?
-- that DOES make sense...

-- So we can do this...  but then we only get access to the "dist" part.
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

iidOn items dist = IIDOn items dist

{-
  could we do i.e.
  strict $ do
    sampling_rate (1/sqrt (fromIntegral n)
    xs <- lazy $ sequence $ repeat dist  -- lazy means no effects (add_move) or rate changes (sample_rate)
    add_move $ interchange_list_entries xs  -- ideally this is only allowed within strict blocks!  And MCMC-specific blocks?
    return $ take n xs
-}

iid2 n dist1 dist2 = iid n $ PairDist dist1 dist2

-- The return type should be Random (Discrete a)
iidMixture n itemDist weightDist = normalizeMixture <$> (sample $ iid2 n (itemDist) (weightDist))

dirichletMixture n a dist = iidMixture n dist (gamma a 1)

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
