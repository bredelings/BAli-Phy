{-# LANGUAGE NoImplicitPrelude #-}
module Main where

data Missing

foreign import trcall "Vector:boxedLength"
    missingTranslation :: Missing -> Int
