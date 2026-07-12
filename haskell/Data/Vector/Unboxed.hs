{-# LANGUAGE NoImplicitPrelude #-}
module Data.Vector.Unboxed
    ( Vector
    , Unbox
    , (!), (!?), unsafeIndex
    , length, null
    , slice, unsafeSlice, take, drop
    , empty, singleton, replicate, fromList, toList
    , zip, unzip
    ) where

import Compiler.Error (error)
import Compiler.Num
import Data.Bool
import Data.Eq
import Data.Maybe (Maybe(Nothing, Just))
import Data.Ord
import Data.Vector.Unboxed.Internal (Vector(..), Unbox(..))
import Foreign.CList (mapFrom)
import Text.Show

infixl 9 !, !?

-- Check a logical index once before dispatching to the representation-specific
-- unchecked read.
(!) :: Unbox a => Vector a -> Int -> a
values ! index
    | index < 0 || index >= length values =
        error "Data.Vector.Unboxed.!: index out of range"
    | otherwise = basicUnsafeIndex values index

-- Return Nothing for indexes outside the logical view without touching its
-- native owner.
(!?) :: Unbox a => Vector a -> Int -> Maybe a
values !? index
    | index < 0 || index >= length values = Nothing
    | otherwise = Just (basicUnsafeIndex values index)

unsafeIndex :: Unbox a => Vector a -> Int -> a
unsafeIndex = basicUnsafeIndex

length :: Unbox a => Vector a -> Int
length = basicLength

null :: Unbox a => Vector a -> Bool
null values = length values == 0

-- Validate a logical subrange without overflowing, then make a metadata-only
-- view of the same unboxed storage.
slice :: Unbox a => Int -> Int -> Vector a -> Vector a
slice start count values
    | start < 0 || count < 0 || start > total || count > total - start =
        error "Data.Vector.Unboxed.slice: invalid slice"
    | otherwise = basicUnsafeSlice start count values
  where
    total = length values

unsafeSlice :: Unbox a => Int -> Int -> Vector a -> Vector a
unsafeSlice = basicUnsafeSlice

-- Clamp a requested prefix length and retain a view of the shared storage.
take :: Unbox a => Int -> Vector a -> Vector a
take requested values = unsafeSlice 0 count values
  where
    count = min (max 0 requested) (length values)

-- Clamp a requested prefix removal and retain the remaining shared-storage
-- view.
drop :: Unbox a => Int -> Vector a -> Vector a
drop requested values = unsafeSlice offset (count - offset) values
  where
    count = length values
    offset = min (max 0 requested) count

empty :: Unbox a => Vector a
empty = fromList []

singleton :: Unbox a => a -> Vector a
singleton = replicate 1

-- NOTE: Keep allocation opaque so simplifying a shared vector cannot duplicate
-- its lazy native owner; remove when the simplifier preserves that sharing.
replicate :: Unbox a => Int -> a -> Vector a
{-# NOINLINE replicate #-}
replicate count value
    | count < 0 = error "Data.Vector.Unboxed.replicate: negative length"
    | otherwise = basicReplicate count value

-- NOTE: Keep allocation opaque so simplifying a shared vector cannot duplicate
-- its lazy native owner; remove when the simplifier preserves that sharing.
fromList :: Unbox a => [a] -> Vector a
{-# NOINLINE fromList #-}
fromList = basicFromList

-- Expose logical elements as a lazy list without allocating a boxed-vector
-- intermediate or copying a primitive slice.
toList :: Unbox a => Vector a -> [a]
toList values = mapFrom 0 (length values) (basicUnsafeIndex values)

-- Truncate to the shorter input and retain normalized O(1) child views.
zip :: (Unbox a, Unbox b) => Vector a -> Vector b -> Vector (a,b)
zip left right = V_2 count (basicUnsafeSlice 0 count left)
                            (basicUnsafeSlice 0 count right)
  where
    count = min (length left) (length right)

unzip :: (Unbox a, Unbox b) => Vector (a,b) -> (Vector a, Vector b)
unzip (V_2 _ left right) = (left, right)

-- Compare corresponding elements without allocating lists, stopping at the
-- first mismatch or at the common end.
vectorsEqual :: (Unbox a, Eq a) => Int -> Int -> Vector a -> Vector a -> Bool
vectorsEqual count index left right
    | index == count = True
    | basicUnsafeIndex left index == basicUnsafeIndex right index =
        vectorsEqual count (index + 1) left right
    | otherwise = False

instance (Unbox a, Eq a) => Eq (Vector a) where
    -- Compare lengths first, then scan equal-sized vectors without lists.
    left == right = leftCount == rightCount &&
                    vectorsEqual leftCount 0 left right
      where
        leftCount = length left
        rightCount = length right

-- Compare logical elements lexicographically and use the lengths only after
-- an equal shared prefix.
compareVectors :: (Unbox a, Ord a) => Vector a -> Vector a -> Ordering
compareVectors left right = go 0
  where
    leftCount = length left
    rightCount = length right
    commonCount = min leftCount rightCount

    -- Scan the shared prefix once and stop at its first unequal element.
    go index
        | index == commonCount = compare leftCount rightCount
        | otherwise = case compare (basicUnsafeIndex left index)
                                   (basicUnsafeIndex right index) of
            EQ -> go (index + 1)
            ordering -> ordering

instance (Unbox a, Ord a) => Ord (Vector a) where
    compare = compareVectors

instance (Unbox a, Show a) => Show (Vector a) where
    show values = show (toList values)
