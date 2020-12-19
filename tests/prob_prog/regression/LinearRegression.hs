import           Probability
import           Data.CSV

xs = map read_double $ head $ read_csv "xs"

ys = map read_double $ head $ read_csv "ys"

prior = do

    b <- normal 0.0 1.0

    a <- normal 0.0 1.0

    sigma <- exponential 1.0

    let loggers = ["b" %=% b, "a" %=% a, "sigma" %=% sigma]

    return (a, b, sigma, loggers)

main = do

    (a, b, sigma, loggers) <- sample $ prior

    let f x = b * x + a

    ys ~> independent [ normal (f x) sigma | x <- xs ]

    return loggers
