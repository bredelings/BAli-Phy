main = do
  let f = \(Just x) -> x
  putStrLn $ show $ f Nothing + 1
