{-# LANGUAGE NoImplicitPrelude #-}
module Data.Array (module Data.Array,
                   module Data.Ix)
    where

import Data.Ix

infixl 9 !
builtin ! 2 "getIndex" "Array"

builtin arraySize 1 "arraySize" "Array"
builtin mkArray 2 "mkArray" "Array"

listArray n l = mkArray n (\i -> l !! i)

listArray' l = listArray (length l) l

