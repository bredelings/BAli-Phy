{-# LANGUAGE NoImplicitPrelude #-}
module System.Environment where

import Foreign.Vector
import Foreign.String
import Compiler.Base (String)
import Compiler.Base (String)
import Compiler.IO
import Compiler.FFI.ToFromC
import Data.List
import Data.Function
import Data.Functor
    
foreign import bpcall "Environment:getArgs" builtin_getArgs :: IO (EVector CPPString)

getArgs :: IO [String]
getArgs = fmap (map listFromString . vectorToList) $ builtin_getArgs

foreign import bpcall "Environment:getArgs" getEnvRaw :: CPPString -> RealWorld -> CPPString

getEnv :: String -> IO String
getEnv = fromC getEnvRaw
