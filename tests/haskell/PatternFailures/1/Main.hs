f (Just x) = x

main = do
  putStrLn $ show $ f Nothing + 1
