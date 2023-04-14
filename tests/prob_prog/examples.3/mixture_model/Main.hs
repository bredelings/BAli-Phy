import Probability
import Data.Frame

generate size = do
  let w     = [0.35, 0.4, 0.25]
      mu    = [0.0, 2.0, 5.0]
      sigma = [0.5, 0.5, 1.0]
  xs <- iid size $ mixture w [ normal m s | (m,s) <- zip mu sigma ]
  return ["xs" %=% xs]


main_generate = generate 1000

model xs = do

  let n_components = 3

  w <- symmetric_dirichlet n_components 1.0
  mu <- sort <$> iid n_components (cauchy 0.0 1.0)
  tau <- iid n_components (gamma 1.0 1.0)

  let loggers = [ "dists" %=% zip w (zip mu tau) ]

  let n_points = length xs

  xs ~> iidDist n_points (mixtureDist w [ normalDist m s | (m, s) <- zip mu tau])

  return loggers

main = do

  frame <- readTable "x.csv"

  let xs = frame $$ "x" :: [Double]

  mcmc $ model xs 
