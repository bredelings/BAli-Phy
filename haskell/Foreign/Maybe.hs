{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Maybe
    ( CMaybe
    , cNothing
    , cJust
    , cIsJust
    , cFromJust
    , cMaybe
    , cIsNothing
    , fromCMaybe
    ) where

import Compiler.FFI.Import (COutput)
import Compiler.FFI.Runtime (RuntimeValue)
import Data.Bool -- for not
import Data.Function -- for (.)
import Data.Maybe

data CMaybe a

-- CMaybe is an opaque runtime result; translating it does not translate a.
instance COutput (CMaybe a)

foreign import ecall "Prelude:" cNothing :: CMaybe a
foreign import ecall "Prelude:cJust" cJustRaw :: a -> CMaybe a

foreign import ecall "Prelude:" cIsJust :: CMaybe a -> Bool
foreign import ecall "Prelude:" cFromJust :: CMaybe a -> a

cJust :: RuntimeValue a => a -> CMaybe a
cJust = cJustRaw

cMaybe :: RuntimeValue a => Maybe a -> CMaybe a
cMaybe Nothing  = cNothing
cMaybe (Just x) = cJust x

cIsNothing = not . cIsJust

fromCMaybe x = if cIsJust x
               then Just $ cFromJust x
               else Nothing
