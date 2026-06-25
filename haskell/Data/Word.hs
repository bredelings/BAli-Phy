{-# LANGUAGE NoImplicitPrelude #-}
module Data.Word where

import Compiler.Num
import Data.Bool
import Data.Eq
import Data.Ord
import Text.Show

type Word = Int

-- FIXME-UNICODE: Word8 is represented by an Int runtime value for now.  A
-- real uint8 runtime value can be considered after profiling runtime plumbing.
data Word8

foreign import ecall "Word:integerToWord8" integerToWord8 :: Integer -> Word8
foreign import ecall "Word:intToWord8" intToWord8 :: Int -> Word8
foreign import ecall "Word:word8ToInt" word8ToInt :: Word8 -> Int

instance Eq Word8 where
    x == y = word8ToInt x == word8ToInt y

instance Ord Word8 where
    x < y = word8ToInt x < word8ToInt y

instance Show Word8 where
    show x = show (word8ToInt x)

instance Num Word8 where
    x + y = intToWord8 (word8ToInt x + word8ToInt y)
    x - y = intToWord8 (word8ToInt x - word8ToInt y)
    x * y = intToWord8 (word8ToInt x * word8ToInt y)
    negate x = intToWord8 (negate (word8ToInt x))
    abs x = x
    signum x = if x == 0 then 0 else 1
    fromInteger = integerToWord8
