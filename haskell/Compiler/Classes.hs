{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Classes where

import Data.Maybe
import Data.Bool
import Data.List

import Text.Show
import Text.Read

deriving instance Show Bool
deriving instance Read Bool

deriving instance Show a => Show (Maybe a)
deriving instance Read a => Read (Maybe a)

