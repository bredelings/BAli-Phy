{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Vector.Unboxed.Internal
    ( Vector(..)
    , Unbox(..)
    , removeMinusOnes
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
    basicConcatN :: Int -> [Vector a] -> Vector a

foreign import bpcall "NativeVector:sizedIntVectorFromList" sizedIntVectorFromListNative :: Int -> [Int] -> NativeVector Int
foreign import bpcall "NativeVector:sizedDoubleVectorFromList" sizedDoubleVectorFromListNative :: Int -> [Double] -> NativeVector Double
foreign import bpcall "NativeVector:vectorKonstNative" intVectorReplicateNative :: Int -> Int -> NativeVector Int
foreign import bpcall "NativeVector:vectorKonstNative" doubleVectorReplicateNative :: Double -> Int -> NativeVector Double
foreign import bpcall "NativeVector:concatIntVectors" concatIntVectorsNative :: Int -> [Vector Int] -> NativeVector Int
foreign import bpcall "NativeVector:concatDoubleVectors" concatDoubleVectorsNative :: Int -> [Vector Double] -> NativeVector Double
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
    basicConcatN count values = V_Int 0 count (concatIntVectorsNative count values)

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
    basicConcatN count values = V_Double 0 count (concatDoubleVectorsNative count values)

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

    -- Concatenate both structure-of-arrays components in the same input order.
    -- Reuse the public calculation of their shared length without forcing either owner.
    basicConcatN count values = V_2 count leftValues rightValues
      where
        leftValues = basicConcatN count (List.map left values)
        rightValues = basicConcatN count (List.map right values)
        left (V_2 _ child _) = child
        right (V_2 _ _ child) = child

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

-- Filter the missing-character sentinel in native code, retaining all other integer values.
foreign import trcall "NativeVector:" removeMinusOnes :: Vector Int -> Vector Int

-- Larger tuples share the pair representation: one logical length and one child
-- vector per field. Slicing and concatenation preserve field order and shared shape.
data instance Vector (a,b,c) = V_3 !Int !(Vector a) !(Vector b) !(Vector c)

instance (Unbox a,Unbox b,Unbox c) => Unbox (a,b,c) where
    basicLength (V_3 count _ _ _) = count
    basicUnsafeIndex (V_3 _ v1 v2 v3) index =
        (basicUnsafeIndex v1 index,
         basicUnsafeIndex v2 index,
         basicUnsafeIndex v3 index)
    -- Slice all children without copying their primitive owners.
    basicUnsafeSlice start count (V_3 _ v1 v2 v3) =
        V_3 count
            (basicUnsafeSlice start count v1)
            (basicUnsafeSlice start count v2)
            (basicUnsafeSlice start count v3)

    -- NOTE: Like the pair fallback, this builds one projected list spine per field.
    -- A direct builder could fill all components without those intermediate lists.
    basicFromListN count values =
        V_3 count
            (basicFromListN count (List.map (\(x,_,_) -> x) values))
            (basicFromListN count (List.map (\(_,x,_) -> x) values))
            (basicFromListN count (List.map (\(_,_,x) -> x) values))

    -- Defer projections until a primitive owner is demanded, preserving shape-only use.
    basicReplicate count value =
        V_3 count
            (basicReplicate count ((\(x,_,_) -> x) value))
            (basicReplicate count ((\(_,x,_) -> x) value))
            (basicReplicate count ((\(_,_,x) -> x) value))

    -- Project and concatenate corresponding children in the same order.
    basicConcatN count values =
        V_3 count
            (basicConcatN count (List.map (\(V_3 _ child _ _) -> child) values))
            (basicConcatN count (List.map (\(V_3 _ _ child _) -> child) values))
            (basicConcatN count (List.map (\(V_3 _ _ _ child) -> child) values))

data instance Vector (a,b,c,d) = V_4 !Int !(Vector a) !(Vector b) !(Vector c) !(Vector d)

instance (Unbox a,Unbox b,Unbox c,Unbox d) => Unbox (a,b,c,d) where
    basicLength (V_4 count _ _ _ _) = count
    basicUnsafeIndex (V_4 _ v1 v2 v3 v4) index =
        (basicUnsafeIndex v1 index,
         basicUnsafeIndex v2 index,
         basicUnsafeIndex v3 index,
         basicUnsafeIndex v4 index)
    -- Slice all children without copying their primitive owners.
    basicUnsafeSlice start count (V_4 _ v1 v2 v3 v4) =
        V_4 count
            (basicUnsafeSlice start count v1)
            (basicUnsafeSlice start count v2)
            (basicUnsafeSlice start count v3)
            (basicUnsafeSlice start count v4)

    -- NOTE: Like the pair fallback, this builds one projected list spine per field.
    -- A direct builder could fill all components without those intermediate lists.
    basicFromListN count values =
        V_4 count
            (basicFromListN count (List.map (\(x,_,_,_) -> x) values))
            (basicFromListN count (List.map (\(_,x,_,_) -> x) values))
            (basicFromListN count (List.map (\(_,_,x,_) -> x) values))
            (basicFromListN count (List.map (\(_,_,_,x) -> x) values))

    -- Defer projections until a primitive owner is demanded, preserving shape-only use.
    basicReplicate count value =
        V_4 count
            (basicReplicate count ((\(x,_,_,_) -> x) value))
            (basicReplicate count ((\(_,x,_,_) -> x) value))
            (basicReplicate count ((\(_,_,x,_) -> x) value))
            (basicReplicate count ((\(_,_,_,x) -> x) value))

    -- Project and concatenate corresponding children in the same order.
    basicConcatN count values =
        V_4 count
            (basicConcatN count (List.map (\(V_4 _ child _ _ _) -> child) values))
            (basicConcatN count (List.map (\(V_4 _ _ child _ _) -> child) values))
            (basicConcatN count (List.map (\(V_4 _ _ _ child _) -> child) values))
            (basicConcatN count (List.map (\(V_4 _ _ _ _ child) -> child) values))

data instance Vector (a,b,c,d,e) = V_5 !Int !(Vector a) !(Vector b) !(Vector c) !(Vector d) !(Vector e)

instance (Unbox a,Unbox b,Unbox c,Unbox d,Unbox e) => Unbox (a,b,c,d,e) where
    basicLength (V_5 count _ _ _ _ _) = count
    basicUnsafeIndex (V_5 _ v1 v2 v3 v4 v5) index =
        (basicUnsafeIndex v1 index,
         basicUnsafeIndex v2 index,
         basicUnsafeIndex v3 index,
         basicUnsafeIndex v4 index,
         basicUnsafeIndex v5 index)
    -- Slice all children without copying their primitive owners.
    basicUnsafeSlice start count (V_5 _ v1 v2 v3 v4 v5) =
        V_5 count
            (basicUnsafeSlice start count v1)
            (basicUnsafeSlice start count v2)
            (basicUnsafeSlice start count v3)
            (basicUnsafeSlice start count v4)
            (basicUnsafeSlice start count v5)

    -- NOTE: Like the pair fallback, this builds one projected list spine per field.
    -- A direct builder could fill all components without those intermediate lists.
    basicFromListN count values =
        V_5 count
            (basicFromListN count (List.map (\(x,_,_,_,_) -> x) values))
            (basicFromListN count (List.map (\(_,x,_,_,_) -> x) values))
            (basicFromListN count (List.map (\(_,_,x,_,_) -> x) values))
            (basicFromListN count (List.map (\(_,_,_,x,_) -> x) values))
            (basicFromListN count (List.map (\(_,_,_,_,x) -> x) values))

    -- Defer projections until a primitive owner is demanded, preserving shape-only use.
    basicReplicate count value =
        V_5 count
            (basicReplicate count ((\(x,_,_,_,_) -> x) value))
            (basicReplicate count ((\(_,x,_,_,_) -> x) value))
            (basicReplicate count ((\(_,_,x,_,_) -> x) value))
            (basicReplicate count ((\(_,_,_,x,_) -> x) value))
            (basicReplicate count ((\(_,_,_,_,x) -> x) value))

    -- Project and concatenate corresponding children in the same order.
    basicConcatN count values =
        V_5 count
            (basicConcatN count (List.map (\(V_5 _ child _ _ _ _) -> child) values))
            (basicConcatN count (List.map (\(V_5 _ _ child _ _ _) -> child) values))
            (basicConcatN count (List.map (\(V_5 _ _ _ child _ _) -> child) values))
            (basicConcatN count (List.map (\(V_5 _ _ _ _ child _) -> child) values))
            (basicConcatN count (List.map (\(V_5 _ _ _ _ _ child) -> child) values))

data instance Vector (a,b,c,d,e,f) = V_6 !Int !(Vector a) !(Vector b) !(Vector c) !(Vector d) !(Vector e) !(Vector f)

instance (Unbox a,Unbox b,Unbox c,Unbox d,Unbox e,Unbox f) => Unbox (a,b,c,d,e,f) where
    basicLength (V_6 count _ _ _ _ _ _) = count
    basicUnsafeIndex (V_6 _ v1 v2 v3 v4 v5 v6) index =
        (basicUnsafeIndex v1 index,
         basicUnsafeIndex v2 index,
         basicUnsafeIndex v3 index,
         basicUnsafeIndex v4 index,
         basicUnsafeIndex v5 index,
         basicUnsafeIndex v6 index)
    -- Slice all children without copying their primitive owners.
    basicUnsafeSlice start count (V_6 _ v1 v2 v3 v4 v5 v6) =
        V_6 count
            (basicUnsafeSlice start count v1)
            (basicUnsafeSlice start count v2)
            (basicUnsafeSlice start count v3)
            (basicUnsafeSlice start count v4)
            (basicUnsafeSlice start count v5)
            (basicUnsafeSlice start count v6)

    -- NOTE: Like the pair fallback, this builds one projected list spine per field.
    -- A direct builder could fill all components without those intermediate lists.
    basicFromListN count values =
        V_6 count
            (basicFromListN count (List.map (\(x,_,_,_,_,_) -> x) values))
            (basicFromListN count (List.map (\(_,x,_,_,_,_) -> x) values))
            (basicFromListN count (List.map (\(_,_,x,_,_,_) -> x) values))
            (basicFromListN count (List.map (\(_,_,_,x,_,_) -> x) values))
            (basicFromListN count (List.map (\(_,_,_,_,x,_) -> x) values))
            (basicFromListN count (List.map (\(_,_,_,_,_,x) -> x) values))

    -- Defer projections until a primitive owner is demanded, preserving shape-only use.
    basicReplicate count value =
        V_6 count
            (basicReplicate count ((\(x,_,_,_,_,_) -> x) value))
            (basicReplicate count ((\(_,x,_,_,_,_) -> x) value))
            (basicReplicate count ((\(_,_,x,_,_,_) -> x) value))
            (basicReplicate count ((\(_,_,_,x,_,_) -> x) value))
            (basicReplicate count ((\(_,_,_,_,x,_) -> x) value))
            (basicReplicate count ((\(_,_,_,_,_,x) -> x) value))

    -- Project and concatenate corresponding children in the same order.
    basicConcatN count values =
        V_6 count
            (basicConcatN count (List.map (\(V_6 _ child _ _ _ _ _) -> child) values))
            (basicConcatN count (List.map (\(V_6 _ _ child _ _ _ _) -> child) values))
            (basicConcatN count (List.map (\(V_6 _ _ _ child _ _ _) -> child) values))
            (basicConcatN count (List.map (\(V_6 _ _ _ _ child _ _) -> child) values))
            (basicConcatN count (List.map (\(V_6 _ _ _ _ _ child _) -> child) values))
            (basicConcatN count (List.map (\(V_6 _ _ _ _ _ _ child) -> child) values))
