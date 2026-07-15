{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Vector.Unboxed.Internal
    ( Vector(..)
    , Unbox(..)
    , intVectorFromNative
    , intVectorFromNativeWithLength
    , intVectorNativeView
    , doubleVectorFromNative
    , doubleVectorFromNativeWithLength
    , doubleVectorNativeView
    ) where

import Compiler.Num
import Compiler.FFI.Import (CInput(..), COutput(..))
import qualified Data.OldList as List
import Data.Ord (min)
import Data.Tuple (fst, snd)
import Foreign.NativeVector (NativeVector)
import Foreign.Pair (EPair, c_fst, c_snd)

data family Vector a

-- Primitive views retain their native owner while keeping stable offset and
-- length metadata in Haskell.
data instance Vector Int = V_Int !Int !Int (NativeVector Int)
data instance Vector Double = V_Double !Int !Int (NativeVector Double)

-- Pair vectors use structure-of-arrays storage and an authoritative logical
-- length shared by the two child views.
data instance Vector (a,b) = V_2 !Int !(Vector a) !(Vector b)

-- Supply only the representation operations needed by the initial immutable
-- interface; constructors and methods remain internal.
class Unbox a where
    basicLength :: Vector a -> Int
    basicUnsafeIndex :: Vector a -> Int -> a
    basicUnsafeSlice :: Int -> Int -> Vector a -> Vector a
    basicFromListN :: Int -> [a] -> Vector a
    basicReplicate :: Int -> a -> Vector a

foreign import bpcall "NativeVector:sizedIntVectorFromList" sizedIntVectorFromListNative :: Int -> [Int] -> NativeVector Int
foreign import bpcall "NativeVector:sizedDoubleVectorFromList" sizedDoubleVectorFromListNative :: Int -> [Double] -> NativeVector Double
foreign import bpcall "NativeVector:vectorKonstNative" intVectorReplicateNative :: Int -> Int -> NativeVector Int
foreign import bpcall "NativeVector:vectorKonstNative" doubleVectorReplicateNative :: Double -> Int -> NativeVector Double
foreign import ecall "NativeVector:unsafeIntIndex" unsafeIntIndexNative :: NativeVector Int -> Int -> Int
foreign import ecall "NativeVector:unsafeDoubleIndex" unsafeDoubleIndexNative :: NativeVector Double -> Int -> Double
foreign import ecall "NativeVector:intVectorSize" intVectorSizeNative :: NativeVector Int -> Int
foreign import ecall "NativeVector:doubleVectorSize" doubleVectorSizeNative :: NativeVector Double -> Int

instance Unbox Int where
    basicLength (V_Int _ count _) = count
    basicUnsafeIndex (V_Int offset _ native) index =
        unsafeIntIndexNative native (offset + index)
    basicUnsafeSlice start count (V_Int offset _ native) =
        V_Int (offset + start) count native
    basicFromListN count values =
        V_Int 0 count (sizedIntVectorFromListNative count values)
    basicReplicate count value =
        V_Int 0 count (intVectorReplicateNative value count)

instance Unbox Double where
    basicLength (V_Double _ count _) = count
    basicUnsafeIndex (V_Double offset _ native) index =
        unsafeDoubleIndexNative native (offset + index)
    basicUnsafeSlice start count (V_Double offset _ native) =
        V_Double (offset + start) count native
    basicFromListN count values =
        V_Double 0 count (sizedDoubleVectorFromListNative count values)
    basicReplicate count value =
        V_Double 0 count (doubleVectorReplicateNative value count)

instance (Unbox a, Unbox b) => Unbox (a,b) where
    basicLength (V_2 count _ _) = count
    basicUnsafeIndex (V_2 _ left right) index =
        (basicUnsafeIndex left index, basicUnsafeIndex right index)
    basicUnsafeSlice start count (V_2 _ left right) =
        V_2 count (basicUnsafeSlice start count left)
                   (basicUnsafeSlice start count right)

    -- NOTE: This immutable fallback builds two projected list spines.  Replace it
    -- when unboxed vectors gain a direct builder that can fill both components.
    basicFromListN count values = V_2 count left right
      where
        left = basicFromListN count (List.map fst values)
        right = basicFromListN count (List.map snd values)

    -- Preserve shape-only operations by deferring both pair projections until
    -- a primitive child owner is actually evaluated.
    basicReplicate count value =
        V_2 count (basicReplicate count (fst value))
                  (basicReplicate count (snd value))

-- Wrap a complete native Int owner and obtain its offset-zero logical length
-- from the owner's physical extent.
intVectorFromNative :: NativeVector Int -> Vector Int
intVectorFromNative native = V_Int 0 (intVectorSizeNative native) native

-- Wrap a complete native Double owner and obtain its offset-zero logical
-- length from the owner's physical extent.
doubleVectorFromNative :: NativeVector Double -> Vector Double
doubleVectorFromNative native =
    V_Double 0 (doubleVectorSizeNative native) native

-- Wrap a native Int owner when an independent Haskell calculation already
-- supplies its authoritative logical length.
intVectorFromNativeWithLength :: Int -> NativeVector Int -> Vector Int
intVectorFromNativeWithLength count = V_Int 0 count

-- Wrap a native Double owner when an independent Haskell calculation already
-- supplies its authoritative logical length.
doubleVectorFromNativeWithLength :: Int -> NativeVector Double -> Vector Double
doubleVectorFromNativeWithLength count = V_Double 0 count

-- Expose the complete primitive view only to foreign-boundary wrappers, so
-- native consumers preserve slices instead of silently using the whole owner.
intVectorNativeView :: Vector Int -> (Int, Int, NativeVector Int)
intVectorNativeView (V_Int offset count native) = (offset, count, native)

-- Expose a Double view only at foreign boundaries, preserving slices through
-- their logical offset and length instead of exposing the complete owner.
doubleVectorNativeView :: Vector Double -> (Int, Int, NativeVector Double)
doubleVectorNativeView (V_Double offset count native) =
    (offset, count, native)

instance CInput (Vector Int) where
    type CInputType (Vector Int) result =
        Int -> Int -> NativeVector Int -> result
    withCInput vector continuation =
        case intVectorNativeView vector of
          (offset, count, owner) -> continuation offset count owner

instance COutput (Vector Int) where
    type COutputType (Vector Int) = NativeVector Int
    fromCOutput = intVectorFromNative

instance CInput (Vector Double) where
    type CInputType (Vector Double) result =
        Int -> Int -> NativeVector Double -> result
    withCInput vector continuation =
        case doubleVectorNativeView vector of
          (offset, count, owner) -> continuation offset count owner

instance (CInput (Vector a), CInput (Vector b)) =>
         CInput (Vector (a,b)) where
    type CInputType (Vector (a,b)) result =
        CInputType (Vector a) (CInputType (Vector b) result)
    withCInput (V_2 _ left right) continuation =
        withCInput right (withCInput left continuation)

instance (Unbox a, Unbox b, COutput (Vector a), COutput (Vector b)) =>
         COutput (Vector (a,b)) where
    type COutputType (Vector (a,b)) =
        EPair (COutputType (Vector a)) (COutputType (Vector b))

    -- Rebuild the structure-of-arrays view and normalize unequal native
    -- owners with the same truncation rule as public vector zip.
    fromCOutput pair =
        V_2 count (basicUnsafeSlice 0 count left)
                   (basicUnsafeSlice 0 count right)
      where
        left = fromCOutput (c_fst pair)
        right = fromCOutput (c_snd pair)
        count = min (basicLength left) (basicLength right)

instance COutput (Vector Double) where
    type COutputType (Vector Double) = NativeVector Double
    fromCOutput = doubleVectorFromNative
