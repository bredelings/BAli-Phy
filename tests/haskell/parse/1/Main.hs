data F123 = F1 { f1,f2::Int, f3::Double} | F2 {f1::Int, f3::Double} | F3 {f2::Int} | F4

main = do
  let x = F1 0 1 2.0
  let y = F2 1 2.0
  putStrLn $ show $ f1 x
  putStrLn $ show $ f1 y
