{-# LANGUAGE NoImplicitPrelude #-}

module Data.Eq (module Data.Bool,
                Eq,
                (==),
                (/=))
    where

import Data.Bool

class Eq a where
    (==) :: a -> a -> Bool

x /= y = not (x == y)
