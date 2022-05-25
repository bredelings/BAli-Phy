{-# LANGUAGE NoImplicitPrelude #-}
module Data.Monoid where

import Data.Semigroup

class Semigroup a => Monoid a where
    mempty :: a
    mappend :: a -> a -> a
    mconcat :: [a] -> a

