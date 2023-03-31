import           Probability

model x = do

    n <- geometric 0.33

    y <- if n > 1 then normal 0.0 1.0 else exponential 1.0

    x ~> normalDist y 1.0

    return ["n" %=% n, "y" %=% y]

main = mcmc $ model 3.0
