module Main where

foreign import bpcall "Prelude:error"
    constrainedBuiltin :: Eq a => a -> a
