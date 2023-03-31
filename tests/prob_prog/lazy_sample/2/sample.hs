import           Probability

model z' = do
    x  <- normal 0.0 1.0
    ys <- lazy $ independent $ repeat $ normal 0.0 1.0
    let zs = (x * x) : (take 10 ys)
    z' ~> normalDist (zs !! 2) 1.0
    return ["zs" %=% zs]

main = do
  mcmc $ model 10.0
