{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ix.Internal
    ( checkedIndex
    , checkedRangeSize
    ) where

import Compiler.Error (error)
import Compiler.Num
import Data.Bool
import Data.Eq
import Data.Ord

-- Convert only non-negative values that are represented exactly by Int.
checkedNonNegativeInt :: [Char] -> Integer -> Int
checkedNonNegativeInt message value =
    let converted = integerToInt value
    in if value < 0 || intToInteger converted /= value
       then error message
       else converted

-- Validate membership before converting an exact Integer offset to Int.
checkedIndex :: Bool -> Integer -> Int
checkedIndex member offset
    | member = checkedNonNegativeInt "Error in array index" offset
    | otherwise = error "Error in array index"

-- Avoid evaluating an exact size for an empty range, and reject sizes that
-- cannot be represented by Int before native allocation.
checkedRangeSize :: Bool -> Integer -> Int
checkedRangeSize nonEmpty size
    | nonEmpty = checkedNonNegativeInt "Negative range size" size
    | otherwise = 0
