{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Enum
import Compiler.Num
import Data.IntSet as I
import Data.Function (($))
import qualified Data.Vector.Unboxed as U
import System.IO (putStrLn)
import Text.Show (show)

-- Exercise ordinary set operations and the unboxed key-vector boundary.
main = do
  let xs = [i | i <- [0..10]]

      m1 = I.fromList xs

      m3 = delete 5 m1

      m4 = insert 100 m3

      m6 = I.fromList [1,2,3]
      m7 = I.fromList [3,4,5]
      unordered = I.fromList [5,-2,7,0,-10]
      sameSetDifferentOrder = I.fromList [0,7,-10,5,-2]

  putStrLn $ show m4
  putStrLn $ show $ union m6 m7
  putStrLn $ show $ intersection m6 m7
  putStrLn $ show $ intersection m7 m6
  putStrLn $ show $ m6 \\ m7
  putStrLn $ show $ m7 \\ m6
  putStrLn $ show $ U.toList $ toVector m4
  putStrLn $ show $ U.length $ toVector empty
  putStrLn $ show $ U.toList $ toAscVector unordered
  putStrLn $ show $ toAscList sameSetDifferentOrder
  putStrLn $ show $ U.toList $ toAscVector empty
