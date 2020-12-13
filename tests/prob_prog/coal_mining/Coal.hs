import Probability

import           Data.ReadFile

times = read_file_as_double "coal-times.dat"
    
truncated min max dist = do x <- dist
                            if ok x then
                                return x
                            else
                                truncated min max dist
    where
      ok x = x >= min && x <= max

orderedSample n dist = do
  xs' <- iid n dist
  let xs = sort xs'
  return $ listArray n xs

prior t1 t2 = do
  let lam = 3.0; nMax = 30; a = 1.0; b = 200.0

  -- n change points, n+1 intervals
  n <- truncated 1 nMax (poisson lam)

  -- even numbered order statistics over the interval [t1,t2]
  s' <- orderedSample (2*n+1) (uniform t1 t2)
  let s = t1:[s'!(2*i-1) | i <- [1..n]]

  -- n+1 rates
  g <- iid (n+1) (gamma a b)

  return (n, s, g)

main = do
  (n, s, g) <- random $ prior 1851.0 1963.0
  
  return [ "n" %=% n, "s" %=% s, "g" %=% g]


