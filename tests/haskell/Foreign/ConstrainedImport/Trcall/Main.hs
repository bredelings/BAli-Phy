module Main where

foreign import trcall "Prelude:error"
    constrainedTranslated :: Eq a => a -> a
