import           Probability
import           Data.ReadFile
import           System.Environment

xs = read_file_as_double "xs"

ys = read_file_as_double "ys"

prior = do

    b <- normal 0.0 1.0

    a <- normal 0.0 1.0

    s <- exponential 1.0

    return (a, b, s)

main = do

    (a, b, s) <- random $ prior

    let f x = b * x + a

    ys ~> independent [ normal (f x) s | x <- xs ]

    return ["b" %=% b, "a" %=% a, "s" %=% s]
