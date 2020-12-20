import Probability
import Data.Frame

generate size = do
  let w     = [0.35, 0.4, 0.25]
      mu    = [0.0, 2.0, 5.0]
      sigma = [0.5, 0.5, 1.0]
  xs <- iid size $ mixture w [ normal m s | (m,s) <- zip mu sigma ]
  return ["xs" %=% xs]


main_generate = sample $ generate 1000


prior n_components = do

  w <- symmetric_dirichlet n_components 1.0
  mu <- sort `liftM` iid n_components (cauchy 0.0 1.0)
  tau <- iid n_components (gamma 1.0 1.0)

  let loggers = [ "dists" %=% zip w (zip mu tau) ]

  return (w, mu, tau, loggers)


main = do
  let observations = (readTable "x.csv") $$ ("x",FDouble)
  (w, mu, tau, loggers) <- sample $ prior 3
  observations ~> iid (length observations) (mixture w [ normal m s | (m, s) <- zip mu tau])
  return loggers
