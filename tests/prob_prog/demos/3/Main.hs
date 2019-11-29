import Probability

main = random $ do
  i <- bernoulli 0.5
  y <- normal 0.0 1.0
  let x = if (i==1) then y else 0.0
  return $ log_all ["i" %=% i, "x" %=% x]

