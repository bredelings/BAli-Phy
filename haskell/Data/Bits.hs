{-# LANGUAGE NoImplicitPrelude #-}
module Data.Bits
    ( Bits(..)
    , FiniteBits(..)
    ) where

import Compiler.Error (error)
import Compiler.Num (negate)
import Data.Eq
import Data.Maybe

infixl 8 `shift`, `rotate`, `shiftL`, `shiftR`, `rotateL`, `rotateR`
infixl 7 .&.
infixl 6 `xor`
infixl 5 .|.

-- Define the standard immutable bit operations, including directional names
-- in terms of the signed shift and rotation operations where possible.
class Eq a => Bits a where
    (.&.), (.|.), xor :: a -> a -> a
    complement :: a -> a

    shift, rotate :: a -> Int -> a
    shiftL, shiftR :: a -> Int -> a
    unsafeShiftL, unsafeShiftR :: a -> Int -> a
    rotateL, rotateR :: a -> Int -> a

    zeroBits :: a
    bit :: Int -> a
    setBit, clearBit, complementBit :: a -> Int -> a
    testBit :: a -> Int -> Bool

    bitSizeMaybe :: a -> Maybe Int
    bitSize :: a -> Int
    isSigned :: a -> Bool
    popCount :: a -> Int

    shiftL value amount = shift value amount
    shiftR value amount = shift value (negate amount)
    unsafeShiftL = shiftL
    unsafeShiftR = shiftR
    rotateL value amount = rotate value amount
    rotateR value amount = rotate value (negate amount)

    bitSize value = case bitSizeMaybe value of
        Just size -> size
        Nothing -> error "Data.Bits.bitSize: undefined"

-- Describe scalar types whose number of meaningful bits is fixed by the type.
-- Variable-length bit vectors intentionally have no FiniteBits instance.
class Bits a => FiniteBits a where
    finiteBitSize :: a -> Int
    countLeadingZeros :: a -> Int
    countTrailingZeros :: a -> Int
