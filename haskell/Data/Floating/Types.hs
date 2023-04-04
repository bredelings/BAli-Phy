{-# LANGUAGE NoImplicitPrelude #-}
module Data.Floating.Types where

foreign import bpcall "Num:" intToDouble :: Int -> Double
foreign import bpcall "Num:" integerToDouble :: Integer -> Double

class FloatConvert a b where
    toFloating :: a -> b

instance FloatConvert Integer Double where
    toFloating  = integerToDouble

instance FloatConvert Int     Double where
    toFloating = intToDouble

instance FloatConvert a       a where
    toFloating x = x
