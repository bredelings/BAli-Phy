import Probability

model = do
  x <- Lazy $ normal 0.0 1.0
  ys <- Lazy $ list (repeat $ normal 0.0 1.0)
  return $ (x*x):(take 10 ys)

main = do
  y <- run_strict (error "No alphabet!") model
  putStrLn $ show y
