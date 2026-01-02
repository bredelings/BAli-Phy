{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Maybe where

import Data.Bool -- for not
import Data.Function -- for (.)
import Data.Maybe
import Compiler.FFI.ToFromC

data CMaybe a

foreign import ecall "Prelude:" cNothing :: CMaybe a
foreign import ecall "Prelude:" cJust :: a -> CMaybe a

foreign import ecall "Prelude:" cIsJust :: CMaybe a -> Bool
foreign import ecall "Prelude:" cFromJust :: CMaybe a -> a

cMaybe Nothing  = cNothing
cMaybe (Just x) = cJust x

cIsNothing = not . cIsJust

fromCMaybe x = if cIsJust x
               then Just $ cFromJust x
               else Nothing

instance ToFromC a => ToFromC (Maybe a) where
    type ToC (Maybe a) = CMaybe (ToC a)

    toC (Just x) = cJust (toC x)
    toC Nothing  = cNothing

    fromC x = if cIsJust x
              then Just $ fromC $ cFromJust x
              else Nothing
