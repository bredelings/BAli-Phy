{-# LANGUAGE NoImplicitPrelude #-}
module ClosedSupport where

type family Imported a where
    Imported Int = Double
    Imported a = Char
