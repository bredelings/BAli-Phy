{-# LANGUAGE NoImplicitPrelude #-}
module Data.List.NonEmpty where

import Data.Eq
import Data.Ord

data NonEmpty a = a :| [a]

instance Eq a => Eq (NonEmpty a) where
    (x :| xs) == (y :| ys) = x == y && xs == ys

instance Ord a => Ord (NonEmpty a) where
    (x :| xs) `compare` (y :| ys) = case x `compare` y of LT -> LT
                                                          GT -> GT
                                                          EQ -> xs `compare` ys
