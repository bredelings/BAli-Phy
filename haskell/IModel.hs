module IModel where

import Probability
import Tree
import Bio.Alignment.Pairwise -- for PairHMM
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

foreign import bpcall "Alignment:" rs05_branch_HMM :: Double -> Double -> Double -> Double -> Bool -> PairHMM
foreign import bpcall "Alignment:rs05_lengthp" builtin_rs05_lengthp :: PairHMM -> Int -> Double
foreign import bpcall "Alignment:" rs07_branch_HMM :: Double -> Double -> Double -> Bool -> PairHMM
foreign import bpcall "Alignment:" multi_rs07_branch_HMM :: Double -> Double -> Double -> Double -> Double -> Double -> Bool -> PairHMM
foreign import bpcall "Alignment:rs07_lengthp" builtin_rs07_lengthp :: Double -> Int -> Double

rs05_lengthp :: PairHMM -> Int -> Log Double
rs05_lengthp m l = toFloating (builtin_rs05_lengthp m l)

rs05 logRate meanIndelLength tau tree = (\d b -> m, rs05_lengthp m) where
      heat = 1.0
      training = False
      rate = exp logRate
      -- Insertions and deletions each have rate 'rate'; x is their combined event probability.
      x = -expm1 (-2.0*rate)
      delta = x/(1.0+x)
      epsilon = (meanIndelLength-1.0)/meanIndelLength
      m = rs05_branch_HMM epsilon delta tau heat training

rs07_lengthp :: Double -> Int -> Log Double
rs07_lengthp e l = toFloating (builtin_rs07_lengthp e l)

rs07 rate mean_length tree = (\ds b ->rs07_branch_HMM epsilon (rate * (ds IntMap.! b)) 1 False, rs07_lengthp epsilon)
    where epsilon = (mean_length-1.0)/mean_length

relaxed_rs07 rate sigma mean_length tree = do
   let branches = getUEdgesSet tree
       epsilon = (mean_length-1.0)/mean_length

   -- If we have a whole bunch of factors with the same sigma, then we can't change sigma
   -- without changing all the factors as well!
   -- Also, its hard to know if the rates cluster in (say) two groups or not.
   factors <- fmap (**sigma) <$> (sample $ iidMap branches $ logNormal 0 1)

   return $ (\ds b -> rs07_branch_HMM epsilon (rate * (ds IntMap.! b) * (factors IntMap.! b)) 1 False, rs07_lengthp epsilon)

-- Cluster branches by the mean and standard deviation of their log rates,
-- retaining an independent standard-normal residual for each branch.
rs07_relaxed_rates_model tree = do
   let branches = IntSet.toAscList (getUEdgesSet tree)
       n_branches = length branches
       delta = 4

   mean <- sample $ iid (n_branches + delta) (laplace (-4.0) (1.0/sqrt 2.0))
   sigma <- sample $ iid (n_branches + delta) (gamma 1.05 0.05)
   alpha <- sample $ gamma 2.0 (1.0/6.0)
   category <- sample $ crp alpha n_branches delta
   z <- sample $ iid n_branches (normal 0.0 1.0)

   -- CRP entries use list positions, whereas branch IDs need not be consecutive.
   -- Sharing a category shares its parameters, not its realized branch rate.
   let rates = IntMap.fromList $ zip branches
                 [exp (mean!!k + residual * sigma!!k) | (k,residual) <- zip category z]

   meanIndelLengthMinus1 <- sample $ exponential 10.0
   let epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1)
   return (\ds b -> rs07_branch_HMM epsilon ((rates IntMap.! b) * (ds IntMap.! b)) 1 False,
           rs07_lengthp epsilon)

multi_rs07 fraction1 rate1 rate2 mean_length tree = (\ds b -> multi_rs07_branch_HMM epsilon fraction1 rate1 rate2 (ds IntMap.! b) 1 False, rs07_lengthp epsilon)
    where epsilon = (mean_length - 1)/mean_length
