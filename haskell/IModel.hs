module IModel where

import Data.Array
import Probability
import Tree
import Bio.Alignment.Pairwise -- for PairHMM
import qualified Data.IntMap as IntMap

foreign import bpcall "Alignment:" rs05_branch_HMM :: Double -> Double -> Double -> Double -> Bool -> PairHMM
foreign import bpcall "Alignment:rs05_lengthp" builtin_rs05_lengthp :: PairHMM -> Int -> Double
foreign import bpcall "Alignment:" rs07_branch_HMM :: Double -> Double -> Double -> Bool -> PairHMM
foreign import bpcall "Alignment:" multi_rs07_branch_HMM :: Double -> Double -> Double -> Double -> Double -> Double -> Bool -> PairHMM
foreign import bpcall "Alignment:rs07_lengthp" builtin_rs07_lengthp :: Double -> Int -> Double

rs05_lengthp m l = doubleToLogDouble (builtin_rs05_lengthp m l)

rs05 logRate meanIndelLength tau tree = (\d b -> m, rs05_lengthp m) where
      heat = 1.0
      training = False
      rate = exp logRate
      x = exp (-2.0*rate)
      delta = x/(1.0+x)
      epsilon = (meanIndelLength-1.0)/meanIndelLength
      m = rs05_branch_HMM epsilon delta tau heat training

rs07_lengthp e l = doubleToLogDouble (builtin_rs07_lengthp e l)

rs07 rate mean_length tree = (\ds b ->rs07_branch_HMM epsilon (rate * (ds IntMap.! b)) 1 False, rs07_lengthp epsilon)
    where epsilon = (mean_length-1.0)/mean_length

relaxed_rs07 rate sigma mean_length tree = do
   let branches = getUEdgesSet tree
       epsilon = (mean_length-1.0)/mean_length

   factors <- sample $ iidMap branches $ log_normal 0 sigma

   return $ (\ds b -> rs07_branch_HMM epsilon (rate * (ds IntMap.! b) * (factors IntMap.! b)) 1 False, rs07_lengthp epsilon)

multi_rs07 fraction1 rate1 rate2 mean_length tree = (\ds b -> multi_rs07_branch_HMM epsilon fraction1 rate1 rate2 (ds IntMap.! b) 1 False, rs07_lengthp epsilon)
    where epsilon = (mean_length - 1)/mean_length
