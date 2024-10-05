module Model where

import Probability
import MCMC

model = do

  -- Default moves have a rate of 0.
  x <- RanSamplingRate 0 $ sample $ normal 0 1

  -- Force x ... so that the transition kernels happen?
  condition (x > 0 || x <= 0)

  -- Try out the generic proposal for atomic objects.
  addMove 10 $ metropolisHastings $ propose x (\x -> logNormal (log x) 1)

  -- Log x
  return ["x" %=% x]

main dir = return model
