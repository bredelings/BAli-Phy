import           Probability
import           Data.ReadFile

xs = read_file_as_double "xs"

ys = read_file_as_double "ys"

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
