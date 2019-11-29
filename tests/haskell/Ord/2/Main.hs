import Data.Set as Set

main = do
  let xs = Set.fromList [1,2,3,4,3]
      ys = Set.fromList [3,4,5,6]
      zs = xs `union` ys
  putStrLn $ show $ toList xs
  putStrLn $ show $ toList ys
  putStrLn $ show $ toList zs
  putStrLn $ show ([1,2,3] > [1,2])
  putStrLn $ show $ toList (xs `intersection` ys)
