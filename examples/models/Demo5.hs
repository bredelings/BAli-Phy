module Demo5 where

import Distributions

random_walk x0 n f | n < 1     = error "Random walk needs at least 1 element"
                   | n == 1    = return [x0]
                   | otherwise = do x1 <- sample $ f x0
                                    xs <- random_walk x1 (n-1) f
                                    return (x0:xs)

-- 20 element brownian bridge
main = do
  zs <- random_walk 0.0 19 (\mu -> normal mu 1.0)

  observe 0.0 $ normal (last zs) 1.0

  return $ log_all [ zs %% "zs" ]
