import           Probability

my_iid n dist =
    let body = do
            x  <- dist
            xs <- my_iid (n - 1) dist
            return (x : xs)
    in  if n == 0 then return [] else body

model = do
    n  <- geometric 0.25
    xs <- my_iid n (normal 0.0 1.0)
    let loggers = ["n" %=% n, "xs" %=% xs]
    return (sum xs, loggers)

main = do
    (total, loggers) <- sample $ model
    20.0 ~> normal total 1.0
    return loggers
