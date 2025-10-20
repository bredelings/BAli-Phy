{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Maybe where

import Data.Bool -- for not
import Data.Function -- for (.)
import Data.Maybe
import Compiler.Translate

data CMaybe a

foreign import bpcall "Prelude:" cNothing :: CMaybe a
foreign import bpcall "Prelude:" cJust :: a -> CMaybe a

foreign import bpcall "Prelude:" cIsJust :: CMaybe a -> Bool
foreign import bpcall "Prelude:" cFromJust :: CMaybe a -> a

cMaybe Nothing  = cNothing
cMaybe (Just x) = cJust x

cIsNothing = not . cIsJust

fromCMaybe x = if cIsJust x
               then Just $ cFromJust x
               else Nothing

instance Translate a => Translate (Maybe a) where
    type Tr (Maybe a) = CMaybe (Tr a)

    toC (Just x) = cJust (toC x)
    toC Nothing  = cNothing

    fromC x = if cIsJust x
              then Just $ fromC $ cFromJust x
              else Nothing
