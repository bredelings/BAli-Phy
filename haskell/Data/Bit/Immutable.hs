{-# LANGUAGE NoImplicitPrelude #-}
module Data.Bit.Immutable where

import Compiler.Error (error)
import Compiler.Integral (mod)
import Compiler.Num
import Data.Bit.Internal
import Data.Bits
import Data.Bool
import Data.Eq
import Data.Function ((.))
import Data.Maybe
import Data.Ord
import qualified Data.Vector.Unboxed as U

foreign import bpcall "Bits:bitwise_or" bitwiseOrNative :: CBitVector -> CBitVector -> CBitVector
foreign import bpcall "Bits:bitwise_and" bitwiseAndNative :: CBitVector -> CBitVector -> CBitVector
foreign import bpcall "Bits:bitwise_xor" bitwiseXorNative :: CBitVector -> CBitVector -> CBitVector
foreign import bpcall "Bits:complement" complementNative :: CBitVector -> CBitVector
foreign import bpcall "Bits:single_bit" singleBitNative :: Int -> CBitVector
foreign import bpcall "Bits:set_bit" setBitNative :: CBitVector -> Int -> CBitVector
foreign import bpcall "Bits:clear_bit" clearBitNative :: CBitVector -> Int -> CBitVector
foreign import bpcall "Bits:shift" shiftNative :: CBitVector -> Int -> CBitVector
foreign import bpcall "Bits:rotate" rotateNative :: CBitVector -> Int -> CBitVector
foreign import bpcall "Bits:popcount" popCountNative :: CBitVector -> Int

instance Bits (U.Vector Bit) where
    left .&. right = bitVectorFromNativeWithLength count
        (bitwiseAndNative (bitVectorNativeOwner left) (bitVectorNativeOwner right))
      where count = min (U.length left) (U.length right)

    left .|. right = bitVectorFromNativeWithLength count
        (bitwiseOrNative (bitVectorNativeOwner left) (bitVectorNativeOwner right))
      where count = min (U.length left) (U.length right)

    left `xor` right = bitVectorFromNativeWithLength count
        (bitwiseXorNative (bitVectorNativeOwner left) (bitVectorNativeOwner right))
      where count = min (U.length left) (U.length right)

    complement vector = bitVectorFromNativeWithLength (U.length vector)
        (complementNative (bitVectorNativeOwner vector))

    -- Upstream treats a bit vector as a sequence: left shifts prefix zeros and
    -- right shifts discard the corresponding low-index elements.
    shift vector amount = bitVectorFromNative (shiftNative (bitVectorNativeOwner vector) amount)

    rotate vector amount
        | U.null vector = vector
        | otherwise = bitVectorFromNativeWithLength (U.length vector)
            (rotateNative (bitVectorNativeOwner vector) (amount `mod` U.length vector))

    zeroBits = U.empty
    bit index
        | index < 0 = U.empty
        | otherwise = bitVectorFromNative (singleBitNative index)

    setBit vector index
        | index < 0 || index >= U.length vector = vector
        | otherwise = bitVectorFromNativeWithLength (U.length vector)
            (setBitNative (bitVectorNativeOwner vector) index)

    clearBit vector index
        | index < 0 || index >= U.length vector = vector
        | otherwise = bitVectorFromNativeWithLength (U.length vector)
            (clearBitNative (bitVectorNativeOwner vector) index)

    complementBit vector index
        | not (testBit vector index) = setBit vector index
        | otherwise = clearBit vector index

    testBit vector index = case vector U.!? index of
        Nothing -> False
        Just (Bit value) -> value

    bitSizeMaybe _ = Nothing
    bitSize _ = error "Data.Bits.bitSize: undefined for U.Vector Bit"
    isSigned _ = False
    popCount = popCountNative . bitVectorNativeOwner
