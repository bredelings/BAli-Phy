import           Probability

model = do
    n <- geometric 0.5
    ys <- iid n (exponential 1)
    3 ~> normalDist (sum ys) 1
    return ["n" %=% n, "ys" %=% ys]

main = do
  mcmc model
