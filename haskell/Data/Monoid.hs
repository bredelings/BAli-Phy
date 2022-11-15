{-# LANGUAGE NoImplicitPrelude #-}
module Data.Monoid where

import Data.Semigroup

class Semigroup a => Monoid a where
    mempty :: a
    mconcat :: [a] -> a

mappend = (<>)
