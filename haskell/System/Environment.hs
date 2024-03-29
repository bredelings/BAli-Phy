module System.Environment where

foreign import bpcall "Environment:getArgs" builtin_getArgs :: IO (EVector CPPString)

getArgs :: IO [String]
getArgs = fmap (map listFromString . list_from_vector) $ builtin_getArgs

