import           Probability
import           Data.Frame

model xs ys = do

    b     <- prior $ normal 0 1

    a     <- prior $ normal 0 1

    sigma <- prior $ exponential 1

    let f x = b * x + a

    observe ys $ independent [ normal (f x) sigma | x <- xs ]

    return ["b" %=% b, "a" %=% a, "sigma" %=% sigma]

main = do
  xy_data <- readTable "xy.csv"

  let xs = xy_data $$ "x" :: [Double]
      ys = xy_data $$ "y" :: [Double]

  return $ model xs ys

