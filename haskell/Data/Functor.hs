{-# LANGUAGE NoImplicitPrelude #-}
module Data.Functor where

import Compiler.Base
import Data.Function

class Functor f

-- This should move to Control.Applicative
pure = return

-- fmap should be a method of `Functor`.
-- fmap should stop mentioning `pure`.
fmap f x = do x' <- x
              pure (f x')

(<$) = fmap . const

($>) = flip (<$)

(<$>) = fmap

(<&>) = flip fmap

void x = () <$ x
