{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Map as Map
import Data.Function (($))
import Data.Ord
import System.IO (putStrLn)
import Text.Show (show)

main = do
  let m = Map.fromList [("A",2),("B",7)] :: Map.Map [Char] Int
  putStrLn (show $ Map.keys m)
  putStrLn (show $ Map.elems m)
  putStrLn (show $ Map.assocs m)
  putStrLn (show $ Map.keySet m)
  putStrLn (show $ m Map.! "A")
