{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Set as Set
import Data.Function (($))
import Data.Ord
import System.IO (putStrLn)
import Text.Show (show)

main = do
  let xs = Set.fromList [1,2,3,4,3] :: Set.Set Int
      ys = Set.fromList [3,4,5,6] :: Set.Set Int
      zs = xs `union` ys
  putStrLn $ show $ toList xs
  putStrLn $ show $ toList ys
  putStrLn $ show $ toList zs
