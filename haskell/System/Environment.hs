module System.Environment where

foreign import bpcall "Environment:getArgs" builtin_getArgs :: IO (EVector CPPString)

getArgs :: IO [String]
getArgs = fmap (map listFromString . vectorToList) $ builtin_getArgs

