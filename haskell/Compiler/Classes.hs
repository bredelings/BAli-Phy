{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Classes where

import Data.Maybe
import Data.Bool
import Data.List

import Text.Show

instance Show Bool where
    show True = "True"
    show False = "False"

instance Show a => Show (Maybe a) where
    show (Just x) = "Just "++show x
    show Nothing = "Nothing"
