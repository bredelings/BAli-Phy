import           Probability

model = do

    p <- sample $ beta 5.0 1.0

    n <- sample $ geometric p

    return ["p" %=% p, "n" %=% n]

main = do
  mcmc model
