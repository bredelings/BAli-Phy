import           Probability
import           Data.Frame

prior = do

    b     <- normal 0.0 1.0

    a     <- normal 0.0 1.0

    sigma <- exponential 1.0

    let loggers = ["b" %=% b, "a" %=% a, "sigma" %=% sigma]

    return (a, b, sigma, loggers)

observe_data xs observed_ys = do

    (a, b, sigma, loggers) <- sample $ prior

    let f x = b * x + a

    observed_ys ~> independent [ normal (f x) sigma | x <- xs ]

    return loggers

main = do
  let xy_data = readTable "xy.csv"
      xs = xy_data $$ ("x", AsDouble)
      ys = xy_data $$ ("y", AsDouble)

  let model = observe_data xs ys

  mcmc model

