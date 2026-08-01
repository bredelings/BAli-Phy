{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Foreign.Vector (EVector)

-- Compile an implicitly generalized translated import whose opaque runtime representation is
-- independent of its element type.
foreign import trcall "Vector:boxedLength" translatedLength :: EVector a -> Int

-- Compile a translated import that shares one rigid type variable between the input and result.
foreign import trcall "Prelude:error" translatedIdentity :: EVector a -> EVector a
