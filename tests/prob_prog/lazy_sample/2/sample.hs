import           Probability

model = do
    x  <- normal 0.0 1.0
    ys <- independent (repeat $ normal 0.0 1.0)
    return $ (x * x) : (take 10 ys)

main = do
    zs <- random $ model
    10.0 ~> normal (zs !! 2) 1.0
    return ["zs" %=% zs]
