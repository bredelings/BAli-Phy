{-# LANGUAGE NoImplicitPrelude #-}
module Data.String where

import Compiler.Base
    
class IsString a where
    fromString :: String -> a

instance a ~ Char => IsString [a] where
    fromString x = x

-- lines :: String -> [String]
-- words :: String -> [String]
-- unlines :: [String] -> String
-- unwords :: [String] -> String
