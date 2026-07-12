{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Enum
import Compiler.Num
import Data.IntMap as I
import qualified Data.IntSet as IS
import Data.Function (($))
import Data.Functor (fmap)
import qualified Data.Vector.Unboxed as U
import System.IO (putStrLn)
import Text.Show (show)

-- Exercise ordinary map operations and the unboxed key-vector boundary.
main = do
  let xs = [(i,i) | i <- [0..10]]

      m1 = I.fromList xs :: IntMap Int

      m2 = fmap (\i -> i*i) m1

      m3 = delete 5 m2

      m4 = insert 100 2 m3

      m5 = insertWith (+) 2 2 m4

      m6 = I.fromList [(1,1),(2,4),(3,9)] :: IntMap Int
      m7 = I.fromList [(3,1), (4,16),(5,25)] :: IntMap Int

      s1 = IS.fromList [1,2,3,4]
      m8 = fromSet (\x -> x*x) s1

  putStrLn $ show m4
  putStrLn $ show m5
  putStrLn $ show $ unionWith (+) m6 m7
  putStrLn $ show $ intersection m6 m7
  putStrLn $ show $ intersection m7 m6
  putStrLn $ show $ intersectionWith (+) m6 m7
  putStrLn $ show $ m6 \\ m7
  putStrLn $ show $ m7 \\ m6
  putStrLn "Set operations"
  putStrLn $ show $ m8
  putStrLn $ show $ keysSet m8
  putStrLn $ show $ forceAll m8
  putStrLn $ show $ U.toList $ keysVector m4
  putStrLn $ show $ U.length $ keysVector (empty :: IntMap Int)


-- maybe make Foldable?
-- folds are ordered though, and IntMap is supposed to be unordered.
-- we could make toList into an IO operation...
