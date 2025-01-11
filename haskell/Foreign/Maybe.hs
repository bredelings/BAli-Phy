{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Maybe where

import Data.Bool -- for not
import Data.Function -- for (.)
import Data.Maybe

data CMaybe a

foreign import bpcall "Prelude:cNothing" builtin_cNothing :: () -> CMaybe a
foreign import bpcall "Prelude:" cJust :: a -> CMaybe a

foreign import bpcall "Prelude:" cIsJust :: CMaybe a -> Bool
foreign import bpcall "Prelude:" cFromJust :: CMaybe a -> a

cMaybe Nothing  = cNothing
cMaybe (Just x) = cJust x

cNothing = builtin_cNothing ()

cIsNothing = not . cIsJust

fromCMaybe x = if cIsJust x
               then Just $ cFromJust x
               else Nothing
