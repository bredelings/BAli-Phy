import Probability

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

model n = do
  r <- poisson 4.0
  l <- if 4 < r
       then return 6
       else do tmp <- poisson 4.0
               return $ fib (2 + r) + tmp
  n ~> (poissonDist $ fromIntegral l)
  return ["r" %=% r]

main = do
  mcmc $ model 6
