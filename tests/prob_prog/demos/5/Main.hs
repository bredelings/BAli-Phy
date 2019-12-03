import Probability

random_walk next x0 = do
  x1 <- next x0
  xs <- random_walk next x1
  return (x0:xs)

random_walk_n n next x0 = do
  xs <- random_walk next x0
  return (take n xs)

-- 20 element brownian bridge
main = do
  zs <- random $ random_walk_n 19 (\mu -> normal mu 1.0) 0.0

  observe (normal (last zs) 1.0) 2.0

  return [ "zs" %=% zs ]
