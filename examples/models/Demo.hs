module Demo where

import Distributions

-- Start from x0, sample n additional points.
-- (f x) is the distribution of the point after x.
random_walk x0 n f | n < 1     = error "Random walk needs at least 1 element"
                   | n == 1    = return [x0]
                   | otherwise = do x1 <- sample $ f x0
                                    xs <- random_walk x1 (n-1) f
                                    return (x0:xs)

main = do

  p <- sample $ beta 10.0 1.0

  n <- sample $ geometric p

  q <- sample $ cauchy 0.0 1.0

  x <- sample $ iid 10 (normal 0.0 1.0)
 
  -- Random array indices.
  -- You can't do this in BUGS: it makes a dynamic graph!
  c <- sample $ iid 10 (categorical (replicate 10 0.1))

  let w = [x!!(c!!i) | i <- [0..9]]

  -- y[i] depends on x[i]
  y <- sample $ list [normal (x!!i) 1.0 | i <- [0..9]]

  -- Brownian-bridge-like
  z <- random_walk 0.0 10 (\mu -> normal mu 1.0)
  observe (normal (last z) 1.0) 2.0

  return $ log_all [ p %% "p",
                     n %% "n",
                     q %% "q",
                     x %% "x",
                     w %% "w",
                     y %% "y",
                     z %% "z" ]
