import           Probability

prior = do
  alpha <- cauchy 0.0 1.0
  beta <- cauchy 0.0 1.0

  -- Poisson regression with mass = e^(a + b*i)
  let dist i = poisson $ safe_exp (alpha + beta * (intToDouble i))

  let loggers = ["alpha" %=% alpha, "beta" %=% beta]

  return (dist, loggers)

observe_data fatalities = do

    (dist, loggers) <- sample $ prior

    fatalities ~> independent [ dist i | i <- [0 .. length fatalities - 1] ]

    return loggers

main = do
  let fatalities = [24, 25, 31, 31, 22, 21, 26, 20, 16, 22]

  let model = observe_data fatalities

  mcmc model
