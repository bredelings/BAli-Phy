import           Probability

my_iid n dist =
    let body = do
            x  <- dist
            xs <- my_iid (n - 1) dist
            return (x : xs)
    in  if n == 0 then return [] else body

prior = do
    n  <- min 100 <$> geometric 0.25
    xs <- my_iid n (normal 0.0 1.0)
    let loggers = ["n" %=% n, "xs" %=% xs]
    return (sum xs, loggers)

observe_data x = do
    (total, loggers) <- sample $ prior
    x ~> normal total 1.0
    return loggers

main = do
  mcmc $ observe_data 20.0
