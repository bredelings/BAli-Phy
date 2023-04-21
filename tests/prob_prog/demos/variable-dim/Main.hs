import           Probability

model = do
    n <- sample $ geometric 0.5
    ys <- sample $ iid n (exponential 1)
    observe 3 $ normal (sum ys) 1
    return ["n" %=% n, "ys" %=% ys]

main = do
  mcmc model
