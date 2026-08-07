{-# LANGUAGE NoImplicitPrelude #-}
module System.Environment where

import Foreign.Vector
import Foreign.String
import Foreign.Maybe ()
import Compiler.Base (String)
import Compiler.IO
import Compiler.FFI.Import
import Data.List
import Data.Maybe
import Data.Function
import Data.Functor
    
foreign import bpcall "Environment:" getArgsRaw :: IO (EVector CPPString)

getArgs :: IO [String]
getArgs = fmap (map listFromString . vectorToList) $ getArgsRaw

type GetEnv = String -> IO String

foreign import bpcall "Environment:" getEnvRaw :: RawImport GetEnv

getEnv :: String -> IO String
getEnv = fromCImport getEnvRaw

type LookupEnv = String -> IO (Maybe String)

foreign import bpcall "Environment:" lookupEnvRaw :: RawImport LookupEnv

lookupEnv :: LookupEnv
lookupEnv = fromCImport lookupEnvRaw

foreign import bpcall "Environment:" getProgNameRaw :: IO CPPString

getProgName :: IO String
getProgName = fmap listFromString getProgNameRaw
