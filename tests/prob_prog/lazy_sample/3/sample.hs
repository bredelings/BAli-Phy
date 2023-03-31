import Probability

model x = do
  x <- normal 0.0 1.0
  ys <- lazy $ independent $ repeat $ normal 0.0 1.0
  let (mu,sigma) = (x, sum $ take 10 $ map (^2) ys)
  x ~> normalDist mu sigma
  return [ "mu" %=% mu, "sigma" %=% sigma]

main = do
  mcmc $ model 1.0
