module System.Environment where

foreign import bpcall "Environment:getArgs" builtin_getArgs :: () -> ()

getArgs = IOAction (\s -> (s, map listFromString $ list_from_vector $ builtin_getArgs ()))

