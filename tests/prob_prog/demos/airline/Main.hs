import           Probability

model fatalities = do

    alpha <- cauchy 0 1
    beta  <- cauchy 0 1

    let loggers = ["alpha" %=% alpha, "beta" %=% beta]

    -- Poisson regression with mass = e^(a + b*i)
    let dist i = poisson $ safe_exp (alpha + beta * (intToDouble i))

    fatalities ~> independent [ dist i | i <- [0 .. length fatalities - 1] ]

    return loggers

main = do
  let fatalities = [24, 25, 31, 31, 22, 21, 26, 20, 16, 22]

  mcmc $ model fatalities
