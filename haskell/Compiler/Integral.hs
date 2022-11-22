{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Integral where

import Compiler.Enum
import Compiler.Real

infixl 7 `quot`, `rem`, `div`, `mod`

class (Real a, Enum a) => Integral a  where { }
-- quot :: a -> a -> a
-- rem  :: a -> a -> a
-- div  :: a -> a -> a
-- mod  :: a -> a -> a
-- quotRem :: a -> a -> (a,a)
-- divMod  :: a -> a -> (a,a)
-- toInteger :: a -> Integer

foreign import bpcall "Prelude:div" div :: a -> a -> a
foreign import bpcall "Prelude:mod" mod :: a -> a -> a
foreign import bpcall "Prelude:quot" quot :: a -> a -> a
foreign import bpcall "Prelude:rem" rem :: a -> a -> a

