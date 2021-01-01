import           Probability

random_walk x1 x2 = do
    dx <- normal 0.0 1.0
    let x3 = dx - x1 + (2.0 * x2)
    xs <- random_walk x2 x3
    return (x1 : xs)

model = sample $ do
    x1   <- normal 0.0 1.0
    x2   <- normal x1 (sqrt $ 1.0 / 3.0)
    walk <- random_walk x1 x2
    let xs = take 100 walk
    return ["x" %=% xs]

main = do
  mcmc model
