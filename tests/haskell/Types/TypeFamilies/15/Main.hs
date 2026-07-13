{-# LANGUAGE NoImplicitPrelude #-}

-- This has the same recursive signature-spine shape needed by RawImport.
type family Normalize f where
    Normalize (input -> rest) = input -> Normalize rest
    Normalize result = result

normalized :: Normalize (Int -> Double -> Char) -> Int -> Double -> Char
normalized x = x
