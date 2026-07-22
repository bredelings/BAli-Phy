module Model where

import Probability
import MCMC (gibbsSampleCategorical)

-- Requires the selected candidate to retain the sampled value that made it possible.
model = do
  let i = modifiable 0
  addMove 1 $ gibbsSampleCategorical i 3
  x <- if i == 0 then return 0 else prior $ uniform 0 1
  condition (i /= 1 || x > 0.5)
  return ["i" %=% i, "x" %=% x]

main _ = return model
