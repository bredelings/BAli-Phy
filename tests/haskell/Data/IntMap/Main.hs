import Data.IntMap as I

main = do
  let xs = [(i,i) | i <- [0..10]]

      m1 = I.fromList xs :: IntMap Int

      m2 = fmap (\i -> i*i) m1

      m3 = delete 5 m2

      m4 = insert 100 2 m3

      m5 = insertWith (+) 2 2 m4

      m6 = I.fromList [(1,1),(2,4),(3,9)] :: IntMap Int
      m7 = I.fromList [(3,1), (4,16),(5,25)] :: IntMap Int

  putStrLn $ show m4
  putStrLn $ show m5
  putStrLn $ show $ unionWith (+) m6 m7
  putStrLn $ show $ intersection m6 m7
  putStrLn $ show $ intersection m7 m6
  putStrLn $ show $ intersectionWith (+) m6 m7
  putStrLn $ show $ m6 \\ m7
  putStrLn $ show $ m7 \\ m6


-- maybe make Foldable?
-- folds are ordered though, and IntMap is supposed to be unordered.
-- we could make toList into an IO operation...
