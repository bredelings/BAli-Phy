{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Maybe where

import Data.Bool -- for not
import Data.Function -- for (.)

data CMaybe a

foreign import bpcall "Prelude:cNothing" builtin_cNothing :: () -> CMaybe a
foreign import bpcall "Prelude:" cJust :: a -> CMaybe a

foreign import bpcall "Prelude:" cIsJust :: CMaybe a -> Bool
foreign import bpcall "Prelude:" cFromJust :: CMaybe a -> a

cNothing = builtin_cNothing ()

cIsNothing = not . cIsJust
