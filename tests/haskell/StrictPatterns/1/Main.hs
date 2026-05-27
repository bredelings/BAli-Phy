f !(Just x) = x

main = do
  putStrLn $ show $ f (Just 3)
