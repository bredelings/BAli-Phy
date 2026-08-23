{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Bit.Internal
    ( Bit(..)
    , CBitVector
    , bitVectorNativeOwner
    ) where

import Compiler.Error (error)
import Compiler.FFI.Import (CInput(..), COutput(..))
import Compiler.FFI.Runtime (RuntimeValue)
import Data.Bits
import Data.Bool
import Data.Eq
import Data.Maybe
import Data.Ord
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal (Vector(..), Unbox(..))
import Text.Show

newtype Bit = Bit { unBit :: Bool } deriving (Eq, Ord)

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

data CBitVector = CBitVector

instance RuntimeValue CBitVector
instance CInput CBitVector
instance COutput CBitVector

-- Cache the logical length while retaining a lazy native owner containing
-- exactly those bits; slices therefore copy instead of storing an offset.
data instance Vector Bit = V_Bit !Int CBitVector

foreign import bpcall "Bits:empty_bitvector" emptyBitVectorNative :: Int -> CBitVector
-- Bit is a newtype, so raw list elements reach C++ as their underlying Bool
-- constructors without allocating a second mapped list spine.
foreign import bpcall "Bits:sized_bitvector_from_list" bitVectorFromListNative :: Int -> [Bit] -> CBitVector
foreign import bpcall "Bits:concat_bitvectors" concatBitVectorsNative :: [Vector Bit] -> CBitVector
foreign import bpcall "Bits:slice" sliceBitVectorNative :: CBitVector -> Int -> Int -> CBitVector
foreign import ecall "Bits:test_bit" testBitVectorNative :: CBitVector -> Int -> Bool
foreign import ecall "Bits:size" bitVectorSizeNative :: CBitVector -> Int
foreign import bpcall "Bits:complement" complementBitVectorNative :: CBitVector -> CBitVector
foreign import bpcall "Bits:bitwise_or" bitwiseOrNative :: CBitVector -> CBitVector -> CBitVector
foreign import bpcall "Bits:bitwise_and" bitwiseAndNative :: CBitVector -> CBitVector -> CBitVector
foreign import bpcall "Bits:bitwise_xor" bitwiseXorNative :: CBitVector -> CBitVector -> CBitVector
foreign import bpcall "Bits:single_bit" singleBitNative :: Int -> CBitVector
foreign import bpcall "Bits:set_bit" setBitNative :: CBitVector -> Int -> CBitVector
foreign import bpcall "Bits:clear_bit" clearBitNative :: CBitVector -> Int -> CBitVector
foreign import bpcall "Bits:shift" shiftNative :: CBitVector -> Int -> CBitVector
foreign import bpcall "Bits:rotate" rotateNative :: CBitVector -> Int -> CBitVector
foreign import bpcall "Bits:popcount" popCountNative :: CBitVector -> Int

instance Unbox Bit where
    basicLength (V_Bit count _) = count
    basicUnsafeIndex (V_Bit _ owner) index = Bit (testBitVectorNative owner index)
    basicUnsafeSlice start count (V_Bit total owner)
        | start == 0 && count == total = V_Bit total owner
        | otherwise = V_Bit count (sliceBitVectorNative owner start count)
    basicFromListN count values = V_Bit count (bitVectorFromListNative count values)
    basicReplicate count (Bit False) = V_Bit count (emptyBitVectorNative count)
    basicReplicate count (Bit True) = V_Bit count (complementBitVectorNative (emptyBitVectorNative count))
    basicConcat values = bitVectorFromNative (concatBitVectorsNative values)

-- Wrap a complete native owner and cache its physical size as the authoritative
-- logical length of the new unboxed vector.
bitVectorFromNative :: CBitVector -> Vector Bit
bitVectorFromNative owner = V_Bit (bitVectorSizeNative owner) owner

-- Expose an exact owner only at foreign boundaries; unlike primitive-vector
-- views, bit vectors never need offset or normalization arguments.
bitVectorNativeOwner :: Vector Bit -> CBitVector
bitVectorNativeOwner (V_Bit _ owner) = owner

instance CInput (Vector Bit) where
    type CInputType (Vector Bit) result = CBitVector -> result
    withCInput vector continuation = continuation (bitVectorNativeOwner vector)

instance COutput (Vector Bit) where
    type COutputType (Vector Bit) = CBitVector
    fromCOutput = bitVectorFromNative

instance Bits (U.Vector Bit) where
    V_Bit leftCount left .&. V_Bit rightCount right =
        V_Bit (min leftCount rightCount) (bitwiseAndNative left right)
    V_Bit leftCount left .|. V_Bit rightCount right =
        V_Bit (min leftCount rightCount) (bitwiseOrNative left right)
    V_Bit leftCount left `xor` V_Bit rightCount right =
        V_Bit (min leftCount rightCount) (bitwiseXorNative left right)

    complement (V_Bit count owner) = V_Bit count (complementBitVectorNative owner)

    -- Upstream treats a bit vector as a sequence: left shifts prefix zeros and
    -- right shifts discard the corresponding low-index elements.
    shift (V_Bit _ owner) amount = bitVectorFromNative (shiftNative owner amount)

    rotate (V_Bit count owner) amount = V_Bit count (rotateNative owner amount)

    zeroBits = U.empty
    bit index
        | index < 0 = U.empty
        | otherwise = bitVectorFromNative (singleBitNative index)

    setBit vector@(V_Bit count owner) index
        | index < 0 || index >= count = vector
        | otherwise = V_Bit count (setBitNative owner index)

    clearBit vector@(V_Bit count owner) index
        | index < 0 || index >= count = vector
        | otherwise = V_Bit count (clearBitNative owner index)

    complementBit vector index
        | not (testBit vector index) = setBit vector index
        | otherwise = clearBit vector index

    testBit (V_Bit count owner) index
        | index < 0 || index >= count = False
        | otherwise = testBitVectorNative owner index

    bitSizeMaybe _ = Nothing
    bitSize _ = error "Data.Bits.bitSize: undefined for U.Vector Bit"
    isSigned _ = False
    popCount (V_Bit _ owner) = popCountNative owner
