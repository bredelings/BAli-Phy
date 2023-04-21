import           Probability

model = do

    xs <- sample $ iid 10 (normal 0 1)

    let ys = map (\x -> x * x) xs

    return ["xs" %=% xs, "squares" %=% ys, "sum" %=% sum ys]

main = do
  mcmc model
