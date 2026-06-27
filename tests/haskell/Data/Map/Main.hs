{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Map as Map
import Data.Function (($))
import Data.Ord
import System.IO (putStrLn)
import Text.Show (show)

main = do
  let m = Map.fromList [("B",7),("A",2),("B",9)] :: Map.Map [Char] Int
      m2 = Map.insert "C" 4 m
      m3 = Map.delete "A" m2
      subSame = Map.fromList [("B",9)] :: Map.Map [Char] Int
      subDifferent = Map.fromList [("B",7)] :: Map.Map [Char] Int
  putStrLn (show $ Map.keys m)
  putStrLn (show $ Map.elems m)
  putStrLn (show $ Map.assocs m)
  putStrLn (show $ Map.keySet m)
  putStrLn (show $ m Map.! "B")
  putStrLn (show $ Map.member "A" m3)
  putStrLn (show $ Map.assocs m3)
  putStrLn (show $ Map.assocs $ Map.map (+1) m)
  putStrLn (show $ Map.isSubmapOf subSame m)
  putStrLn (show $ Map.isSubmapOf subDifferent m)
  putStrLn (show $ Map.isProperSubmapOf subSame m)
  putStrLn (show $ Map.isProperSubmapOf m m)
  putStrLn (show $ Map.toAscList (Map.fromDistinctAscList [("A",1),("B",2),("C",3)] :: Map.Map [Char] Int))
  putStrLn (show $ Map.toAscList (Map.fromDistinctDescList [("C",3),("B",2),("A",1)] :: Map.Map [Char] Int))
  putStrLn (show $ Map.toAscList (Map.fromAscList [("A",1),("A",2),("B",3)] :: Map.Map [Char] Int))
