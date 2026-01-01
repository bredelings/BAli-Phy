{-# LANGUAGE NoImplicitPrelude #-}
module Data.Floating.Types where

foreign import ecall "Num:" intToDouble :: Int -> Double
foreign import ecall "Num:" integerToDouble :: Integer -> Double

class FloatConvert a b where
    toFloating :: a -> b

instance FloatConvert Integer Double where
    toFloating  = integerToDouble

instance FloatConvert Int     Double where
    toFloating = intToDouble

instance {-# INCOHERENT #-} FloatConvert a       a where
    toFloating x = x

{- NOTE: Problems with defaults.

The geometric distribution is currently specified as:

  geometric :: Double -> Geometric
  geometric pSuccess = Geometric (toFloating pSuccess)

We need to specify that pSuccess is Double because otherwise
expressions like (geometric p) would have an ambiguous type
such as (forall a.FloatConvert a Prob => a)

If we could add a (Num a) constraint and avoid disabling defaulting
when constraints from outside the standard library are used, then
we could default to Double.

Alternatively, if the hypothetical extension to multi-parameter
type-classes from the NamedDefaults proposal was ever proposed
and implemented, then we could do something like:

   default FloatConvert a b => {a ~ b}

That would default to Prob instead.  Hmm...

-}
