{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification #-}

import Data.Eq
import Data.Bool

data AnyEq = forall a. Eq a => AE a

reflexive :: AnyEq -> Bool
reflexive (AE x) = x == x

reflexive2 :: AnyEq -> Bool
reflexive2 ae = x == x
  where
  AE x = ae
