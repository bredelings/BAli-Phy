import Probability
import Data.Frame

cluster_dist = do
  mean <- normal 0.0 10.0
  prec <- gamma 1.0 1.0
  let sigma = 1.0/prec
  return (mean,sigma)

prior n_points = do

  alpha <- gamma 0.5 10.0

  params <- dp n_points alpha cluster_dist

  let loggers = ["alpha" %=% alpha, "params" %=% params]

  return (alpha, params, loggers)

observe_data xs = do

  let n_points = length xs

  (alpha, params, loggers) <- sample $ prior n_points

  xs ~> independent [normal mean sigma | (mean,sigma) <- params]

  return loggers


main = do

  let xs = readTable "x.csv" $$ ("x", AsDouble)

  let model = observe_data xs

  mcmc model

