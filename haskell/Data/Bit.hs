{-# LANGUAGE NoImplicitPrelude #-}
module Data.Bit
    ( Bit(..)
    ) where

import Data.Bits
import Data.Bool
import Data.Eq
import Data.Maybe
import Data.Ord
import Text.Show

newtype Bit = Bit { unBit :: Bool }

instance Eq Bit where
    Bit left == Bit right = left == right

instance Ord Bit where
    compare (Bit left) (Bit right) = compare left right

instance Show Bit where
    show (Bit False) = "0"
    show (Bit True) = "1"

instance Bits Bit where
    Bit left .&. Bit right = Bit (left && right)
    Bit left .|. Bit right = Bit (left || right)
    Bit left `xor` Bit right = Bit (left /= right)
    complement (Bit value) = Bit (not value)

    shift value 0 = value
    shift _ _ = Bit False
    rotate value _ = value

    zeroBits = Bit False
    bit 0 = Bit True
    bit _ = Bit False

    setBit _ 0 = Bit True
    setBit value _ = value
    clearBit _ 0 = Bit False
    clearBit value _ = value
    complementBit (Bit value) 0 = Bit (not value)
    complementBit value _ = value
    testBit (Bit value) 0 = value
    testBit _ _ = False

    bitSizeMaybe _ = Just 1
    bitSize _ = 1
    isSigned _ = False
    popCount (Bit False) = 0
    popCount (Bit True) = 1

instance FiniteBits Bit where
    finiteBitSize _ = 1
    countLeadingZeros (Bit False) = 1
    countLeadingZeros (Bit True) = 0
    countTrailingZeros (Bit False) = 1
    countTrailingZeros (Bit True) = 0
