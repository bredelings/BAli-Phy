{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Foreign.CList (CList, listToCList)
import Foreign.Pair (EPair, c_pair, pair_from_c)
import Foreign.Vector (EVector, clist_to_vector, listToVector, vectorToList)
import Data.List (map)
import System.IO (print)

main = do
    print (pair_from_c pair)
    print (vectorToList integers)
    print (map pair_from_c (vectorToList pairs))
    print (vectorToList (clist_to_vector integersCList))
  where
    pair = c_pair 3 4 :: EPair Int Int
    integers = listToVector [1,2,3] :: EVector Int
    pairs = listToVector [c_pair 5 6] :: EVector (EPair Int Int)
    integersCList = listToCList [7,8] :: CList Int
