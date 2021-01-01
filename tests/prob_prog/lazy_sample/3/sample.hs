import Probability

prior = do
  x <- normal 0.0 1.0
  ys <- independent (repeat $ normal 0.0 1.0)
  return (x, sum $ take 10 $ map (^2) ys)

observe_data x = do
  (mu,sigma) <- sample $ prior
  x ~> normal mu sigma
  return [ "mu" %=% mu, "sigma" %=% sigma]

main = do
  let model = observe_data 1.0
  mcmc model
