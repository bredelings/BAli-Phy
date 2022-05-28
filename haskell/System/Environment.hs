module System.Environment where

foreign import bpcall "Environment:getArgs" builtin_getArgs 1

getArgs = IOAction (\s -> (s, map listFromString $ list_from_vector $ builtin_getArgs ()))

