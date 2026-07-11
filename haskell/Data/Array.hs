{-# LANGUAGE NoImplicitPrelude #-}
module Data.Array
    ( Array
    , array, listArray
    , (!), (//)
    , accum, accumArray, ixmap
    , bounds, numElements
    , indices, elems, assocs
    , module Data.Ix
    ) where

import Compiler.Error (error)
import Compiler.Num
import Data.Bool
import Data.Eq
import qualified Data.Foldable as F
import Data.Function
import Data.Functor
import Data.Ix
import Data.Ord
import Data.Traversable
import qualified Data.Vector as Vector
import qualified Data.Vector.Internal as Vector
import Text.Show

type role Array nominal representational
data Array i e = Array !i !i !Int !(Vector.Vector e)

infixl 9 !, //

-- Convert an array index to a stored offset only after checking both the Ix
-- range and the independently stored element count.
safeIndex :: Ix i => (i,i) -> Int -> i -> Int
safeIndex arrayBounds count value
    | not (inRange arrayBounds value) = error "Data.Array: index out of range"
    | offset < 0 || offset >= count = error "Data.Array: invalid Ix offset"
    | otherwise = offset
  where offset = index arrayBounds value

-- Lazily translate public Ix associations to checked linear offsets while
-- leaving each associated value unevaluated for the native consumer.
linearizeAssociations :: Ix i => (i,i) -> Int -> [(i,a)] -> [(Int,a)]
linearizeAssociations arrayBounds count associations =
    [(safeIndex arrayBounds count arrayIndex, value) |
     (arrayIndex,value) <- associations]

{-# NOINLINE undefinedArrayElement #-}
undefinedArrayElement :: e
undefinedArrayElement = error "(Array.!): undefined array element"

-- Construct an immutable array by translating arbitrary Ix associations to
-- checked linear offsets consumed incrementally by the native vector builder.
array :: Ix i => (i,i) -> [(i,e)] -> Array i e
array arrayBounds@(lower,upper) associations =
    Array lower upper count
        (Vector.fromIndexedList count undefinedArrayElement
            (linearizeAssociations arrayBounds count associations))
  where
    count = rangeSize arrayBounds

-- Fill an immutable array in range order, sharing one undefined thunk for a
-- short input and leaving any excess list tail unevaluated.
listArray :: Ix i => (i,i) -> [e] -> Array i e
listArray arrayBounds@(lower,upper) values =
    Array lower upper count
        (Vector.fromListNDefault count undefinedArrayElement values)
  where count = rangeSize arrayBounds

(!) :: Ix i => Array i e -> i -> e
Array lower upper count values ! arrayIndex =
    values Vector.! safeIndex (lower,upper) count arrayIndex

-- Replace checked array positions in one native vector copy, retaining lazy
-- update values and allowing the last duplicate association to win.
(//) :: Ix i => Array i e -> [(i,e)] -> Array i e
values@(Array lower upper count elements) // associations =
    Array lower upper count
        (Vector.replaceIndexed elements
            (linearizeAssociations (bounds values) count associations))

-- Accumulate checked associations in source order while preserving the
-- original bounds and the native operation's WHNF result strictness.
accum :: Ix i => (e -> a -> e) -> Array i e -> [(i,a)] -> Array i e
accum combine values@(Array lower upper count elements) associations =
    Array lower upper count
        (Vector.accumIndexed combine elements
            (linearizeAssociations (bounds values) count associations))

-- Initialize every slot lazily, then accumulate associations in source order
-- using the same checked offset conversion as existing-array updates.
accumArray :: Ix i => (e -> a -> e) -> e -> (i,i) -> [(i,a)] -> Array i e
accumArray combine initial arrayBounds@(lower,upper) associations =
    Array lower upper count
        (Vector.accumIndexed combine (Vector.replicate count initial)
            (linearizeAssociations arrayBounds count associations))
  where count = rangeSize arrayBounds

-- Enumerate target indices once and retain each mapped source lookup lazily;
-- array checks every target Ix value without repeated list indexing.
ixmap :: (Ix i, Ix j) => (i,i) -> (i -> j) -> Array j e -> Array i e
ixmap targetBounds mapping source =
    array targetBounds
        [(target, source ! mapping target) | target <- range targetBounds]

bounds :: Array i e -> (i,i)
bounds (Array lower upper _ _) = (lower,upper)

numElements :: Array i e -> Int
numElements (Array _ _ count _) = count

indices :: Ix i => Array i e -> [i]
indices = range . bounds

elems :: Array i e -> [e]
elems (Array _ _ _ values) = Vector.toList values

assocs :: Ix i => Array i e -> [(i,e)]
assocs values = [(arrayIndex, values ! arrayIndex) |
                 arrayIndex <- indices values]

instance Functor (Array i) where
    fmap function (Array lower upper count values) =
        Array lower upper count (Vector.map function values)

instance F.Foldable (Array i) where
    fold (Array _ _ _ values) = F.fold values
    foldMap function (Array _ _ _ values) = F.foldMap function values
    foldMap' function (Array _ _ _ values) = F.foldMap' function values
    foldr function initial (Array _ _ _ values) = F.foldr function initial values
    foldr' function initial (Array _ _ _ values) = F.foldr' function initial values
    foldl function initial (Array _ _ _ values) = F.foldl function initial values
    foldl' function initial (Array _ _ _ values) = F.foldl' function initial values
    foldr1 function (Array _ _ _ values) = F.foldr1 function values
    foldl1 function (Array _ _ _ values) = F.foldl1 function values
    toList = elems
    null values = numElements values == 0
    length = numElements
    elem target (Array _ _ _ values) = F.elem target values

instance Traversable (Array i) where
    traverse function (Array lower upper count values) =
        Array lower upper count <$> traverse function values

-- Match standard array equality: bounds distinguish nonempty arrays, while
-- all empty arrays compare equal regardless of their reverse bounds.
instance (Ix i, Eq e) => Eq (Array i e) where
    Array leftLower leftUpper leftCount leftValues ==
        Array rightLower rightUpper rightCount rightValues
        | leftCount /= rightCount = False
        | leftCount == 0 = True
        | otherwise = (leftLower,leftUpper) == (rightLower,rightUpper) &&
                      leftValues == rightValues

instance (Ix i, Ord e) => Ord (Array i e) where
    compare left right = compare (assocs left) (assocs right)

instance (Ix i, Show i, Show e) => Show (Array i e) where
    -- Render array as a prefix application and parenthesize it when nested in
    -- another constructor application.
    showsPrec precedence values = showParen (precedence > 10) $
        showString "array " . shows (bounds values) . showChar ' ' .
        shows (assocs values)
