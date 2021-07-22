import           Probability

model = do
    x  <- normal 0.0 1.0
    ys <- lazy $ independent $ repeat $ normal 0.0 1.0
    return $ (x * x) : (take 10 ys)

main = do
    y <- run_strict $ normal 0.0 1.0
    putStrLn $ show y
