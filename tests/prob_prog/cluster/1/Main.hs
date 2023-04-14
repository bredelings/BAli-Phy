import Probability
import System.Environment
import Data.Frame

-- Ideally, the categories and their weights would be exchangeable!
-- Currently if the first category is bad, there is not a good way
-- to eliminate it.

cluster_dist = do
  mean <- normal 0.0 10.0
  prec <- gamma 2.0 1.0
  let sigma = 1.0/prec
  return (mean,sigma)

model xs = do

  let n_points = length xs

  alpha <- gamma 0.5 10.0

  params <- dp n_points alpha cluster_dist

  xs ~> independentDist [normalDist mean sigma | (mean,sigma) <- params]

  let loggers = ["alpha" %=% alpha, "params" %=% params]

  return loggers


main = do
  [filename] <- getArgs

  xtable <- readTable filename

  let xs = xtable $$ "x" :: [Double]

  mcmc $ model xs

