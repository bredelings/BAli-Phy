{-# LANGUAGE NoImplicitPrelude #-}
module Data.Exception where

import Data.Typeable -- for Typeable
import Text.Show     -- for Show
import Data.Maybe    -- for Maybe
import Compiler.Base -- for String
import Data.List     -- for ++
import Compiler.IO   -- for IO

data SomeException = forall e. Exception e => SomeException e

class ({-Typeable e,-} Show e) => Exception e where
--    toException :: e -> SomeException
--    fromException :: SomeException -> Maybe e
    displayException :: e -> String
    displayException e = show e

data IOException = IOException String

instance Show IOException where
    show (IOException s) = "IOException " ++ show s

instance Exception IOException
--    toException e = SomeException e
--    fromException e = Just e

data ArithException

data ArrayExeption


--throw :: forall a e. Exception e => e -> a
foreign import bpcall "Prelude:" throw :: IOException -> b

{-
throwIO :: Exception e => e -> IO a
-}

--catch :: Exception e => IO a -> (e -> IO a) -> IO a
foreign import bpcall "Prelude:" catchRaw :: a -> (IOException -> a) -> a
catch :: IO a -> (IOException -> IO a) -> IO a
catch action handler = IO (\s -> catchRaw (runIO action s) (\e -> runIO (handler e) s))

