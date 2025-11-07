{-# LANGUAGE NoImplicitPrelude #-}
module Data.Semigroup where

import Data.List.NonEmpty
import Data.Ord    

infixr 6 <>

class Semigroup a where
    (<>) :: a -> a -> a
    x <> y = sconcat ( x :| [y] )

    sconcat :: NonEmpty a -> a
    sconcat (x :| xs) = go x xs where
                  go x [] = x
                  go x (y:ys) = x <> go y ys

--  We probably need to split the NonEmpty definition into a different file
--  (maybe Compiler.NonEmpty) to avoid module loops.

--  stimes :: Integral b => b -> a -> a

instance Semigroup () where
    _ <> _ = ()
    sconcat _ = ()

instance (Semigroup a, Semigroup b) => Semigroup (a,b) where
    (x,y) <> (x',y') = (x <> x', y <> y')

instance Semigroup Ordering where
    EQ <> y = y
    x  <> _ = x
                    
