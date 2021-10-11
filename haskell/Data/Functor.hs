{-# LANGUAGE NoImplicitPrelude #-}
module Data.Functor where

import Compiler.Base
import Data.Function

class Functor f

pure = return

fmap f x = do x' <- x
              pure (f x')

a <$ bs = do bs
             pure a

($>) = flip (<$)

(<$>) = fmap

(<&>) = flip fmap

void as = do as
             pure ()
