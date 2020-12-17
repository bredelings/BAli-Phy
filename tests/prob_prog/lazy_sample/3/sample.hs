import Probability

model = do
  x <- normal 0.0 1.0
  ys <- independent (repeat $ normal 0.0 1.0)
  return (x, sum $ take 10 $ map (^2) ys)

main = do
  (mu,sigma) <- random $ model
  1.0 ~> normal mu sigma
  return [ "mu" %=% mu, "sigma" %=% sigma]
