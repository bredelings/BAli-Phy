module Probability.Distribution.Markov where

import Probability.Random

sample_markov x0 next = do
  x1 <- next x0
  xs <- sample_markov x1 next
  return (x0:xs)

sample_markov_n 0 x0 next = return []
sample_markov_n n x0 next = do
  x1 <- next x0
  xs <- sample_markov_n (n-1) x1 next
  return (x0:xs)


-- I think if next has type (a->Dist a) and not just
-- (a->Sample a), then we could compute likelihoods.

-- The fastest way would be to represent the transition
-- probabilities in a matrix -- at least for integer
-- sequences -- but that is less general.  Maybe we should
-- make a markov_int distribution that takes a starting
-- distribution and a transition probability matrix?

-- I presume that we would need to make any markov DISTRIBUTION
-- have an actual length.  Do we want them to be lazy?  Strict?
