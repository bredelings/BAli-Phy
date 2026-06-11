{-# LANGUAGE NoImplicitPrelude #-}
module Data.List.NonEmpty where

import Data.Eq
import Data.Ord

data NonEmpty a = a :| [a] deriving (Eq, Ord)
