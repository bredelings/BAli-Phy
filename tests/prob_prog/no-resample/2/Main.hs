import           Probability

prior = do
    n <- geometric 0.33
    y <- if n > 1 then normal 0.0 1.0 else exponential 1.0
    let loggers = ["n" %=% n, "y" %=% y]
    return (y,loggers)

observe_data x = do
    (y,loggers) <- sample $ prior

    x ~> normal y 1.0

    return loggers

main = do
  mcmc $ observe_data 3.0
