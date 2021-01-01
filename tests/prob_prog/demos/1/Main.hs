import           Probability

model = sample $ do

    p <- beta 5.0 1.0

    n <- geometric p

    return ["p" %=% p, "n" %=% n]

main = do
  mcmc model
