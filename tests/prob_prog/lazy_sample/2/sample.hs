import           Probability

prior = do
    x  <- normal 0.0 1.0
    ys <- independent (repeat $ normal 0.0 1.0)
    return $ (x * x) : (take 10 ys)

observe_data z' = do
    zs <- sample $ prior
    z' ~> normal (zs !! 2) 1.0
    return ["zs" %=% zs]

main = do
  let model = observe_data 10.0
  mcmc model
