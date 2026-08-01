{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Foreign.Vector (EVector)

-- Compile an implicitly generalized translated import whose opaque runtime representation is independent of its element type.
foreign import trcall "Vector:boxedLength" translatedLength :: EVector a -> Int
