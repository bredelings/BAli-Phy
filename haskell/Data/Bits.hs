{-# LANGUAGE NoImplicitPrelude #-}
module Data.Bits
    ( Bits(..)
    , FiniteBits(..)
    ) where

import Compiler.Error (error)
import Compiler.Num (Num(..))
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

-- Int uses the width of the runtime's native Int, which need not be the pointer width.
instance Bits Int where
    (.&.) = andInt
    (.|.) = orInt
    xor = xorInt
    complement = complementInt
    shift = shiftInt
    shiftL = shiftLInt
    shiftR = shiftRInt
    rotate = rotateInt
    rotateR = rotateRInt
    zeroBits = 0
    bit = shiftLInt 1
    setBit x i = x .|. bit i
    clearBit x i = x .&. complement (bit i)
    complementBit x i = xor x (bit i)
    testBit x i = (x .&. bit i) /= 0
    bitSizeMaybe _ = Just intBitSize
    isSigned _ = True
    popCount = popCountInt

instance FiniteBits Int where
    finiteBitSize _ = intBitSize
    countLeadingZeros = countLeadingZerosInt
    countTrailingZeros = countTrailingZerosInt

-- Integer has infinite sign extension; rotation is therefore a shift, without wraparound.
-- As in GHC, popCount on a negative Integer is minus the population count of its magnitude.
instance Bits Integer where
    (.&.) = andInteger
    (.|.) = orInteger
    xor = xorInteger
    complement = complementInteger
    shift = shiftInteger
    shiftL = shiftLInteger
    shiftR = shiftRInteger
    rotate = shiftInteger
    rotateR = rotateRInteger
    zeroBits = 0
    bit = shiftLInteger 1
    setBit x i = x .|. bit i
    clearBit x i = x .&. complement (bit i)
    complementBit x i = xor x (bit i)
    testBit = testBitInteger
    bitSizeMaybe _ = Nothing
    isSigned _ = True
    popCount = popCountInteger

foreign import ecall "Bits:" andInt :: Int -> Int -> Int
foreign import ecall "Bits:" orInt :: Int -> Int -> Int
foreign import ecall "Bits:" xorInt :: Int -> Int -> Int
foreign import ecall "Bits:" complementInt :: Int -> Int
foreign import ecall "Bits:" shiftInt :: Int -> Int -> Int
foreign import ecall "Bits:" shiftLInt :: Int -> Int -> Int
foreign import ecall "Bits:" shiftRInt :: Int -> Int -> Int
foreign import ecall "Bits:" rotateRInt :: Int -> Int -> Int
foreign import ecall "Bits:" rotateInt :: Int -> Int -> Int
foreign import ecall "Bits:" popCountInt :: Int -> Int
foreign import ecall "Bits:" andInteger :: Integer -> Integer -> Integer
foreign import ecall "Bits:" orInteger :: Integer -> Integer -> Integer
foreign import ecall "Bits:" xorInteger :: Integer -> Integer -> Integer
foreign import ecall "Bits:" complementInteger :: Integer -> Integer
foreign import ecall "Bits:" shiftInteger :: Integer -> Int -> Integer
foreign import ecall "Bits:" shiftLInteger :: Integer -> Int -> Integer
foreign import ecall "Bits:" shiftRInteger :: Integer -> Int -> Integer
foreign import ecall "Bits:" rotateRInteger :: Integer -> Int -> Integer
foreign import ecall "Bits:" popCountInteger :: Integer -> Int
foreign import ecall "Bits:" intBitSize :: Int
foreign import ecall "Bits:" countLeadingZerosInt :: Int -> Int
foreign import ecall "Bits:" countTrailingZerosInt :: Int -> Int
foreign import ecall "Bits:" testBitInteger :: Integer -> Int -> Bool
