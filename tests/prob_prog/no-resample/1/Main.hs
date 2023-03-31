import           Probability

my_iid n dist =
    let body = do
            x  <- dist
            xs <- my_iid (n - 1) dist
            return (x : xs)
    in  if n == 0 then return [] else body

model x = do
    n  <- min 100 <$> geometric 0.25
    xs <- lazy $ my_iid n (normal 0.0 1.0)
    let total = sum xs
    let loggers = ["n" %=% n, "xs" %=% xs]
    x ~> normalDist total 1.0
    return loggers

main = do
  mcmc $ model 20.0
