import           Probability
import           Data.Frame

model xs ys = do

    b     <- normal 0.0 1.0

    a     <- normal 0.0 1.0

    sigma <- exponential 1.0

    let f x = b * x + a

    ys ~> independent [ normal (f x) sigma | x <- xs ]

    let loggers = ["b" %=% b, "a" %=% a, "sigma" %=% sigma]

    return loggers

main = do
  xy_data <- readTable "xy.csv"

  let xs = xy_data $$ "x" :: [Double]
      ys = xy_data $$ "y" :: [Double]

  mcmc $ model xs ys

