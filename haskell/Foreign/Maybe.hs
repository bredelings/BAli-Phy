{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Maybe where

import Compiler.FFI.Import (COutput)
import Data.Bool -- for not
import Data.Function -- for (.)
import Data.Maybe

data CMaybe a

-- CMaybe is an opaque runtime result; translating it does not translate a.
instance COutput (CMaybe a)

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
