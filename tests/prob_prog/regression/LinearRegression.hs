import           Probability
import           Data.Frame

xy_data = readTable "xy.csv"
xs = xy_data $$ ("x", AsDouble)
ys = xy_data $$ ("y", AsDouble)

prior = do

    b     <- normal 0.0 1.0

    a     <- normal 0.0 1.0

    sigma <- exponential 1.0

    let loggers = ["b" %=% b, "a" %=% a, "sigma" %=% sigma]

    return (a, b, sigma, loggers)

main = do

    (a, b, sigma, loggers) <- sample $ prior

    let f x = b * x + a

    ys ~> independent [ normal (f x) sigma | x <- xs ]

    return loggers
