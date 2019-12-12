import Probability

model = do
  x <- normal 0.0 1.0
  ys <- independent (repeat $ normal 0.0 1.0)
  return (x, sum $ take 10 $ map (^2) ys)

main = do
  (mu,sigma) <- random $ model
  observe (normal mu sigma) 1.0
  return [ "mu" %=% mu, "sigma" %=% sigma]
