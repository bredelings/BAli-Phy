{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import ClosedSupport

importedFirst :: Imported Int -> Double
importedFirst x = x

importedFallback :: Imported Double -> Char
importedFallback x = x
