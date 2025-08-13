{-# LANGUAGE NoImplicitPrelude #-}
module Data.String (
   String,
   IsString(..),
   lines,
   words,
   unlines,
   unwords)
where

import Compiler.Base (String)
import Data.OldList (lines, words, unlines, unwords)
    
class IsString a where
    fromString :: String -> a

instance a ~ Char => IsString [a] where
    fromString x = x
