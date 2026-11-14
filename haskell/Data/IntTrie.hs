-- Vendored from data-inttrie 0.1.4; implementation unchanged.
-- https://hackage.haskell.org/package/data-inttrie-0.1.4
-- See IntTrie.LICENSE for the upstream license.
--
-- The upstream signed-key traversal negates negative keys. For Int, minBound cannot
-- be negated without overflow; use Integer keys when that value must be represented.
-------------------------------------------------------------
-- |
-- Module        : Data.IntTrie
-- Copyright     : (c) Luke Palmer 2010
-- License       : BSD3
--
-- Maintainer    : Luke Palmer <lrpalmer@gmail.com>
-- Stability     : experimental
-- Portability   : Haskell 2010
--
-- Provides a minimal infinite, lazy trie for integral types.
-- It intentionally leaves out ideas such as delete and
-- emptiness so that it can be used lazily, eg. as the target
-- of an infinite foldr.  Essentially its purpose is to be an
-- efficient implementation of a function from integral type,
-- given point-at-a-time modifications.
-------------------------------------------------------------

module Data.IntTrie 
    ( IntTrie, identity, apply, modify, modify', overwrite,
      mirror, modifyAscList, modifyDescList )
where

import Control.Applicative
import Control.Arrow (first, second)
import Data.Bits
import Data.Function (fix)
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))

-- | A trie from integers to values of type a. 
-- 
-- Semantics: [[IntTrie a]] = Integer -> a
data IntTrie a = IntTrie (BitTrie a) a (BitTrie a)  -- negative, 0, positive

data BitTrie a = BitTrie a (BitTrie a) (BitTrie a)

instance Functor BitTrie where
    fmap f ~(BitTrie x l r) = BitTrie (f x) (fmap f l) (fmap f r)

instance Applicative BitTrie where
    pure x = fix (\g -> BitTrie x g g)
    ~(BitTrie f fl fr) <*> ~(BitTrie x xl xr) = BitTrie (f x) (fl <*> xl) (fr <*> xr)

instance Semigroup a => Semigroup (BitTrie a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (BitTrie a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance Functor IntTrie where
    fmap f ~(IntTrie neg z pos) = IntTrie (fmap f neg) (f z) (fmap f pos)

instance Applicative IntTrie where
    pure x = IntTrie (pure x) x (pure x)
    IntTrie fneg fz fpos <*> IntTrie xneg xz xpos = 
        IntTrie (fneg <*> xneg) (fz xz) (fpos <*> xpos)

instance Semigroup a => Semigroup (IntTrie a) where
    (<>) = liftA2 (<>)

instance Monoid a => Monoid (IntTrie a) where
    mempty = pure mempty
    mappend = liftA2 mappend

-- | Apply the trie to an argument.  This is the semantic map.
apply :: (Ord b, Num b, Bits b) => IntTrie a -> b -> a
apply (IntTrie neg z pos) x =
    case compare x 0 of
        LT -> applyPositive neg (-x)
        EQ -> z
        GT -> applyPositive pos x

applyPositive :: (Num b, Bits b) => BitTrie a -> b -> a
applyPositive (BitTrie one even odd) x
    | x == 1 = one
    | testBit x 0 = applyPositive odd (x `shiftR` 1)
    | otherwise   = applyPositive even (x `shiftR` 1)

-- | The identity trie.  
--
-- > apply identity = id
identity :: (Num a, Bits a) => IntTrie a
identity = IntTrie (fmap negate identityPositive) 0 identityPositive

identityPositive :: (Num a, Bits a) => BitTrie a
identityPositive = go
    where
    go = BitTrie 1 (fmap (`shiftL` 1) go) (fmap (\n -> (n `shiftL` 1) .|. 1) go)

-- | Modify the function at one point
--
-- > apply (modify x f t) i | i == x = f (apply t i)
-- >                        | otherwise = apply t i
modify :: (Ord b, Num b, Bits b) => b -> (a -> a) -> IntTrie a -> IntTrie a
modify x f ~(IntTrie neg z pos) =
    case compare x 0 of
        LT -> IntTrie (modifyPositive (-x) f neg) z pos
        EQ -> IntTrie neg (f z) pos
        GT -> IntTrie neg z (modifyPositive x f pos)

modifyPositive :: (Num b, Bits b) => b -> (a -> a) -> BitTrie a -> BitTrie a
modifyPositive x f ~(BitTrie one even odd)
    | x == 1      = BitTrie (f one) even odd
    | testBit x 0 = BitTrie one even (modifyPositive (x `shiftR` 1) f odd)
    | otherwise   = BitTrie one (modifyPositive (x `shiftR` 1) f even) odd


-- | Modify the function at one point (strict version)
modify' :: (Ord b, Num b, Bits b) => b -> (a -> a) -> IntTrie a -> IntTrie a
modify' x f (IntTrie neg z pos) =
    case compare x 0 of
        LT -> (IntTrie $! modifyPositive' (-x) f neg) z pos
        EQ -> (IntTrie neg $! f z) pos
        GT -> IntTrie neg z $! modifyPositive' x f pos

modifyPositive' :: (Num b, Bits b) => b -> (a -> a) -> BitTrie a -> BitTrie a
modifyPositive' x f (BitTrie one even odd)
    | x == 1      = (BitTrie $! f one) even odd
    | testBit x 0 = BitTrie one even $! modifyPositive' (x `shiftR` 1) f odd
    | otherwise   = (BitTrie one $! modifyPositive' (x `shiftR` 1) f even) odd


-- | Overwrite the function at one point
--
-- > overwrite i x = modify i (const x)
overwrite :: (Ord b, Num b, Bits b) => b -> a -> IntTrie a -> IntTrie a
overwrite i x = modify i (const x)


-- | Negate the domain of the function
--
-- > apply (mirror t) i = apply t (-i)
-- > mirror . mirror = id
mirror :: IntTrie a -> IntTrie a
mirror ~(IntTrie neg z pos) = IntTrie pos z neg


-- | Modify the function at a (potentially infinite) list of points in ascending order
--
-- > modifyAscList [(i0, f0)..(iN, fN)] = modify i0 f0 . ... . modify iN fN
modifyAscList :: (Ord b, Num b, Bits b) => [(b, a -> a)] -> IntTrie a -> IntTrie a
modifyAscList ifs ~t@(IntTrie neg z pos) =
    case break ((>= 0) . fst) ifs of
        ([],   [])          -> t
        (nifs, (0, f):pifs) -> IntTrie (modifyAscListNegative nifs neg) (f z)
                                       (modifyAscListPositive pifs pos)
        (nifs, pifs)        -> IntTrie (modifyAscListNegative nifs neg) z
                                       (modifyAscListPositive pifs pos)
    where modifyAscListNegative = modifyAscListPositive . map (first negate) . reverse

-- | Modify the function at a (potentially infinite) list of points in descending order
modifyDescList :: (Ord b, Num b, Bits b) => [(b, a -> a)] -> IntTrie a -> IntTrie a
modifyDescList ifs = mirror . modifyAscList (map (first negate) ifs) . mirror

modifyAscListPositive :: (Ord b, Num b, Bits b) => [(b, a -> a)] -> BitTrie a -> BitTrie a
modifyAscListPositive [] t = t
modifyAscListPositive ((0, _):_) _ =
    error "modifyAscList: expected strictly monotonic indices"
modifyAscListPositive ifs@((i, f):_) ~(BitTrie one even odd) = BitTrie one' even' odd' where
    (one', ifs')      = if i == 1 then (f one, tail ifs) else (one, ifs)
    even'             = modifyAscListPositive ifsEven even
    odd'              = modifyAscListPositive ifsOdd  odd
    (ifsOdd, ifsEven) = both (map $ first (`shiftR` 1)) $ partitionIndices ifs'
    both f (x, y)     = (f x, f y)

-- Like `partition (flip testBit 0 . fst)`, except that this version addresses the
-- problem of infinite lists of only odd or only even indices by injecting an `id`
-- into the other result list wherever there are two evens or two odds in a row.
-- This allows `modifyAscListPositive` to return a value as soon as the next index is
-- higher than the current location in the trie instead of scanning for the end of
-- the list, which for infinite lists may never be reached.
partitionIndices :: (Num b, Bits b) => [(b, a -> a)] -> ([(b, a -> a)], [(b, a -> a)])
partitionIndices []           = ([], [])
partitionIndices [x]          = if testBit (fst x) 0 then ([x], []) else ([], [x])
partitionIndices (x:xs@(y:_)) = case testBit (fst x) 0 of
    False -> (if testBit (fst y) 0 then odd else pad:odd, x:even)
    True  -> (x:odd, if testBit (fst y) 0 then pad:even else even)
    where ~(odd, even) = partitionIndices xs
          pad = (fst y - 1, id)
