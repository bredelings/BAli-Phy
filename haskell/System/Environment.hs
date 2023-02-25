module System.Environment where

foreign import bpcall "Environment:getArgs" builtin_getArgs :: RealWorld -> EVector CPPString

getArgs = makeIO (\s -> map listFromString $ list_from_vector $ builtin_getArgs s)

