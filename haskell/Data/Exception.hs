{-# LANGUAGE NoImplicitPrelude #-}
module Data.Exception where

import Data.Typeable -- for Typeable
import Text.Show     -- for Show
import Data.Maybe    -- for Maybe
import Compiler.Base -- for String

data SomeException = forall e. Exception e => SomeException e

class (Typeable e, Show e) => Exception e where
    toException :: e -> SomeException
    fromException :: SomeException -> Maybe e
    displayException :: e -> String

data IOException

data ArithException

data ArrayExeption

