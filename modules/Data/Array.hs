{-# LANGUAGE NoImplicitPrelude #-}
module Data.Array where

import Data.Bool

builtin arraySize 1 "arraySize" "Array"
builtin ! 2 "getIndex" "Array"
builtin mkArray 2 "mkArray" "Array"

listArray n l = mkArray n (\i -> l !! i)

listArray' l = listArray (length l) l

