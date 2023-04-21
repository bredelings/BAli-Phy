import           Probability

model = do
    i <- prior $ bernoulli 0.5
    y <- prior $ normal 0.0 1.0
    let x = if (i == 1) then y else 0.0
    return ["i" %=% i, "x" %=% x]

main = do
  mcmc model
      
