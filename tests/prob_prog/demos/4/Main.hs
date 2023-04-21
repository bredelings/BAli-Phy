import           Probability

model = do

    xs         <- prior $ iid 10 (normal 0.0 1.0)

    categories <- prior $ iid 10 (categorical (replicate 10 0.1))

    let ys = [ xs !! (categories !! i) | i <- [0 .. 9] ]
    return ["ys" %=% ys]

main = do
  mcmc model
