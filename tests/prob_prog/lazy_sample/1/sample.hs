import           Probability

model = do
    x  <- sample $ normal 0 1
    ys <- lazy $ sample $ independent $ repeat $ normal 0 1
    return $ (x * x) : (take 10 ys)

main = do
    y <- run_strict $ sample $ normal 0 1
    putStrLn $ show y
