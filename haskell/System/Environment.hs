module System.Environment where

builtin builtin_getArgs 1 "getArgs" "Environment"

getArgs = map listFromString $ list_from_vector $ builtin_getArgs ()

