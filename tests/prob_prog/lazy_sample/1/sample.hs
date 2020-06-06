import           Probability

model = random $ do
    x  <- normal 0.0 1.0
    ys <- independent (repeat $ normal 0.0 1.0)
    return $ (x * x) : (take 10 ys)

main = do
    y <- run_strict model
    putStrLn $ show y
