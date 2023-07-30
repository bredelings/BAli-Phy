import           Probability

model z' = do
    x  <- sample $ normal 0 1
    ys <- lazy $ sample $ independent $ repeat $ normal 0 1
    let zs = (x * x) : (take 10 ys)
    observe z' $ normal (zs !! 2) 1
    return ["zs" %=% zs]

main = do
  return $ model 10
