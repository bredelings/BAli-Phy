module IModel where

import Probability
import Tree
import Bio.Alignment.Pairwise -- for PairHMM

foreign import bpcall "Alignment:rs05_branch_HMM" rs05_branch_HMM :: Double -> Double -> Double -> Double -> Bool -> PairHMM
foreign import bpcall "Alignment:rs05_lengthp" builtin_rs05_lengthp :: PairHMM -> Int -> Double
foreign import bpcall "Alignment:rs07_branch_HMM" rs07_branch_HMM :: Double -> Double -> Double -> Bool -> PairHMM
foreign import bpcall "Alignment:rs07_lengthp" builtin_rs07_lengthp :: Double -> Double -> Double

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

rs07 rate mean_length tree = (\d b ->rs07_branch_HMM epsilon (rate * d!b) 1.0 False, rs07_lengthp epsilon)
    where epsilon = (mean_length-1.0)/mean_length

rs07_relaxed_rates_model tree = do 
   let n_branches = numBranches tree
       delta = 4

   mean <- iid (n_branches + delta) (laplace (-4.0) (1.0/sqrt 2.0))
   sigma <- iid (n_branches + delta) (gamma 1.05 0.05)
  
   alpha <- gamma 2.0 (1.0/6.0)

   category <- RanDistribution $ crp alpha n_branches delta

   z <- iid n_branches (normal 0.0 1.0)

   let logLambdas = [ mean!!k + z!!i * sigma!!k | i <- take n_branches [0..], let k=category!!i]

   meanIndelLengthMinus1 <- exponential 10.0
    
   let epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1)
       lambdas = map exp logLambdas

   return $ (\d b -> rs07_branch_HMM epsilon (lambdas!!b * d!b), \l -> rs07_lengthp epsilon l)
