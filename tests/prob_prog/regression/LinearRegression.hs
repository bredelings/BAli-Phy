import           Probability
import           Data.Frame

model xs ys = do

    b     <- normal 0 1

    a     <- normal 0 1

    sigma <- exponential 1

    let f x = b * x + a

    ys ~> independent [ normal (f x) sigma | x <- xs ]

    return ["b" %=% b, "a" %=% a, "sigma" %=% sigma]

main = do
  xy_data <- readTable "xy.csv"

  let xs = xy_data $$ "x" :: [Double]
      ys = xy_data $$ "y" :: [Double]

  mcmc $ model xs ys

