import Probability
import Data.Frame

cluster_dist = do
  mean <- normal 0.0 10.0
  prec <- gamma 1.0 1.0
  let sigma = 1.0/prec
  return (mean,sigma)

model xs = do

  let n_points = length xs

  alpha <- gamma 0.5 10.0

  params <- dp n_points alpha cluster_dist

  xs ~> independent [normal mean sigma | (mean,sigma) <- params]

  let loggers = ["alpha" %=% alpha, "params" %=% params]

  return loggers


main = do

  xtable <- readTable "x.csv"

  let xs = xtable $$ ("x", AsDouble)

  mcmc $ model xs

