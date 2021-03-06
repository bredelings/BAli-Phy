import Probability

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

prior = do
  r <- poisson 4.0
  l <- if 4 < r
       then return 6
       else do tmp <- poisson 4.0
               return $ fib (2 + r) + tmp
  return (r,l)

observe_data n = do
  (r,l) <- sample $ prior
  n ~> (poisson $ intToDouble l)
  return ["r" %=% r]

main = do
  let model = observe_data 6

  mcmc model
