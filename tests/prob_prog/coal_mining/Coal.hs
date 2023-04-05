import Probability
import Data.Frame
import Data.Array

-- When we get polymorphism, maybe we can create an temporary array as a view on xs'
-- I presume that using n directly is more efficient that using [length xs']
orderedSample n dist = do
  xs' <- iid n dist
  let xs = sort xs'
  return $ listArray n xs

get_intervals (r:[]) (t:[]) tend = [(r,t,tend)]
get_intervals (r:rs) (t1:ts@(t2:_)) tend = (r,t1,t2):get_intervals rs ts tend

model (t1,t2) times = do
  let lam = 3; nMax = 30; a = 1; b = 365/200

  -- n change points, n+1 intervals
  n <- min nMax <$> poisson lam

  -- even numbered order statistics over the interval [t1,t2]
  s' <- orderedSample (2*n+1) (uniform t1 t2)
  let s = t1:[s'!(2*i-1) | i <- [1..n]]

  -- n+1 rates
  g <- iid (n+1) (gamma a b)

  let intervals = get_intervals g s t2

  times ~> poisson_processes intervals

  return [ "n" %=% n, "s" %=% s, "g" %=% g, "intervals" %=% intervals]


main = do

  frame <- readTable "coal-times.csv"

  let times = frame $$ "time" :: [Double]

  mcmc $ model (1851, 1963) times
