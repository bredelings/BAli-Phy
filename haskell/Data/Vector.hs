{-# LANGUAGE NoImplicitPrelude #-}
module Data.Vector
    ( Vector
    , (!), (!?)
    , empty, singleton, generate, replicate
    , fromList, toList
    , length, null
    , map, imap
    , elemIndex
    , slice, take, drop, (++)
    ) where

import Compiler.Base (seq)
import Compiler.Num
import Data.Bool
import Data.Eq
import qualified Data.Foldable as F
import Data.Function
import Data.Functor
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import Foreign.CList (mapFrom)
import Text.Show

type role Vector representational
data Vector a

infixl 9 !, !?
infixr 5 ++

foreign import bpcall "Vector:boxedGenerate" generate :: Int -> (Int -> a) -> Vector a
foreign import bpcall "Vector:boxedReplicate" replicate :: Int -> a -> Vector a
foreign import bpcall "Vector:boxedFromList" vectorFromList :: [a] -> Vector a
foreign import bpcall "Vector:boxedLength" vectorLength :: Vector a -> Int
foreign import bpcall "Vector:boxedIndex" (!) :: Vector a -> Int -> a
foreign import bpcall "Vector:boxedSlice" slice :: Int -> Int -> Vector a -> Vector a
foreign import bpcall "Vector:boxedAppend" (++) :: Vector a -> Vector a -> Vector a

empty :: Vector a
empty = fromList []

fromList :: [a] -> Vector a
fromList = vectorFromList

singleton :: a -> Vector a
singleton = replicate 1

length :: Vector a -> Int
length = vectorLength

null :: Vector a -> Bool
null values = length values == 0

(!?) :: Vector a -> Int -> Maybe a
values !? index
    | index < 0 || index >= length values = Nothing
    | otherwise = Just (values ! index)

-- Expose vector elements as a lazy list while retaining indexed access to the
-- shared vector payload.
toList :: Vector a -> [a]
toList = vectorToList

vectorToList :: Vector a -> [a]
vectorToList values = mapFrom 0 (length values) (\index -> values ! index)

map :: (a -> b) -> Vector a -> Vector b
map function values = generate (length values) (\index -> function (values ! index))

imap :: (Int -> a -> b) -> Vector a -> Vector b
imap function values = generate (length values) (\index -> function index (values ! index))

take :: Int -> Vector a -> Vector a
take count values = slice 0 (min (max 0 count) (length values)) values

drop :: Int -> Vector a -> Vector a
drop count values = slice offset (length values - offset) values
  where offset = min (max 0 count) (length values)

-- Return the first index whose element equals the requested value, without
-- converting the vector to a list.
elemIndex :: Eq a => a -> Vector a -> Maybe Int
elemIndex target values = go 0
  where
    -- Scan indexes in ascending order and stop at the first match.
    go index
        | index == length values = Nothing
        | values ! index == target = Just index
        | otherwise = go (index + 1)

instance Functor Vector where
    fmap = map

instance F.Foldable Vector where
    fold = F.foldr (<>) mempty
    foldMap function = F.foldr (\value result -> function value <> result) mempty
    foldMap' function = F.foldl' (\result value -> result <> function value) mempty
    toList = vectorToList
    length = vectorLength
    null values = vectorLength values == 0

    -- Fold from the left without first allocating a list of vector elements.
    foldl function initial values = go 0 initial
      where
        -- Carry a lazy accumulator through the vector in index order.
        go index result
            | index < vectorLength values = go (index + 1) (function result (values ! index))
            | otherwise = result

    -- Strictly fold from the left while retaining indexed vector traversal.
    foldl' function initial values = go 0 initial
      where
        -- Force each accumulator before advancing to the next index.
        go index result
            | index < vectorLength values =
                let next = function result (values ! index)
                in next `seq` go (index + 1) next
            | otherwise = result

    -- Fold from the right without materializing a separate element list.
    foldr function initial values = go 0
      where
        -- Produce a lazy right-associated fold beginning at this index.
        go index
            | index < vectorLength values = function (values ! index) (go (index + 1))
            | otherwise = initial

    -- Strictly fold from the right without first materializing a list.
    foldr' function initial values = go (vectorLength values - 1) initial
      where
        -- Force each suffix accumulator while walking indexes downward.
        go index result
            | index >= 0 =
                let next = function (values ! index) result
                in next `seq` go (index - 1) next
            | otherwise = result

    -- Fold a nonempty vector from the left, reporting the usual empty error
    -- through vector indexing when no initial element exists.
    foldl1 function values = go 1 (values ! 0)
      where
        -- Carry a lazy nonempty accumulator through the remaining indexes.
        go index result
            | index < vectorLength values = go (index + 1) (function result (values ! index))
            | otherwise = result

    -- Fold a nonempty vector from the right, retaining lazy right-association.
    foldr1 function values = go 0
      where
        lastIndex = vectorLength values - 1
        -- Right-associate elements up to the final vector element.
        go index
            | index < lastIndex = function (values ! index) (go (index + 1))
            | otherwise = values ! lastIndex

    elem target values = case elemIndex target values of
        Nothing -> False
        Just _ -> True

-- Compare corresponding elements without allocating lists, stopping at the
-- first mismatch or at the end of the shorter vector.
vectorsEqual :: Eq a => Int -> Vector a -> Vector a -> Bool
vectorsEqual index left right
    | index == length left = True
    | left ! index == right ! index = vectorsEqual (index + 1) left right
    | otherwise = False

instance Eq a => Eq (Vector a) where
    left == right = length left == length right && vectorsEqual 0 left right

-- Lexicographically compare vector elements before comparing a shared prefix's
-- lengths, matching the conventional boxed-vector ordering.
compareVectors :: Ord a => Int -> Vector a -> Vector a -> Ordering
compareVectors index left right
    | index == commonLength = compare (length left) (length right)
    | otherwise = case compare (left ! index) (right ! index) of
        EQ -> compareVectors (index + 1) left right
        ordering -> ordering
  where commonLength = min (length left) (length right)

instance Ord a => Ord (Vector a) where
    compare = compareVectors 0

instance Show a => Show (Vector a) where
    show values = show (toList values)
