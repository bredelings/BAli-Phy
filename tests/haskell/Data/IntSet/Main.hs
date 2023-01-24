import Data.IntSet as I

main = do
  let xs = [i | i <- [0..10]]

      m1 = I.fromList xs

      m3 = delete 5 m1

      m4 = insert 100 m3

      m6 = I.fromList [1,2,3]
      m7 = I.fromList [3,4,5]

  putStrLn $ show m4
  putStrLn $ show $ union m6 m7
  putStrLn $ show $ intersection m6 m7
  putStrLn $ show $ intersection m7 m6
  putStrLn $ show $ m6 \\ m7
  putStrLn $ show $ m7 \\ m6
