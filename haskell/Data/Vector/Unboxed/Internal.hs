{-# LANGUAGE NoImplicitPrelude #-}
module Data.Vector.Unboxed.Internal
    ( Vector(..)
    , Unbox(..)
    , intVectorFromNative
    , doubleVectorFromNative
    ) where

import Compiler.Num
import qualified Data.OldList as List
import Data.Tuple (fst, snd)
import Foreign.NativeVector (NativeVector)

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
    basicFromList :: [a] -> Vector a
    basicReplicate :: Int -> a -> Vector a

foreign import bpcall "NativeVector:intVectorFromList" intVectorFromListNative :: [Int] -> NativeVector Int
foreign import bpcall "NativeVector:doubleVectorFromList" doubleVectorFromListNative :: [Double] -> NativeVector Double
foreign import bpcall "NativeVector:vectorKonstNative" intVectorReplicateNative :: Int -> Int -> NativeVector Int
foreign import bpcall "NativeVector:vectorKonstNative" doubleVectorReplicateNative :: Double -> Int -> NativeVector Double
foreign import ecall "NativeVector:unsafeIntIndex" unsafeIntIndexNative :: NativeVector Int -> Int -> Int
foreign import ecall "NativeVector:unsafeDoubleIndex" unsafeDoubleIndexNative :: NativeVector Double -> Int -> Double

instance Unbox Int where
    basicLength (V_Int _ count _) = count
    basicUnsafeIndex (V_Int offset _ native) index =
        unsafeIntIndexNative native (offset + index)
    basicUnsafeSlice start count (V_Int offset _ native) =
        V_Int (offset + start) count native
    basicFromList values =
        V_Int 0 (List.length values) (intVectorFromListNative values)
    basicReplicate count value =
        V_Int 0 count (intVectorReplicateNative value count)

instance Unbox Double where
    basicLength (V_Double _ count _) = count
    basicUnsafeIndex (V_Double offset _ native) index =
        unsafeDoubleIndexNative native (offset + index)
    basicUnsafeSlice start count (V_Double offset _ native) =
        V_Double (offset + start) count native
    basicFromList values =
        V_Double 0 (List.length values) (doubleVectorFromListNative values)
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
    basicFromList values = V_2 count left right
      where
        left = basicFromList (List.map fst values)
        right = basicFromList (List.map snd values)
        count = basicLength left

    -- Preserve shape-only operations by deferring both pair projections until
    -- a primitive child owner is actually evaluated.
    basicReplicate count value =
        V_2 count (basicReplicate count (fst value))
                  (basicReplicate count (snd value))

-- Wrap a trusted native Int producer with stable, offset-zero Haskell shape
-- metadata.  The supplied count must match the native extent.
intVectorFromNative :: Int -> NativeVector Int -> Vector Int
intVectorFromNative count = V_Int 0 count

-- Wrap a trusted native Double producer with stable, offset-zero Haskell shape
-- metadata.  The supplied count must match the native extent.
doubleVectorFromNative :: Int -> NativeVector Double -> Vector Double
doubleVectorFromNative count = V_Double 0 count
