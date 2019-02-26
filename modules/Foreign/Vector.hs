{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where

import Compiler.Num
import Data.Ord

builtin builtin_set_vector_index 3 "set_vector_index" "Vector"
builtin builtin_new_vector 1 "new_vector" "Vector"
builtin get_vector_index 2 "get_vector_index" "Vector"
builtin vector_size 1 "vector_size" "Vector"
builtin sizeOfVectorUnsigned 1 "sizeOfVectorUnsigned" "Vector"
builtin sizeOfVectorInt 1 "sizeOfVectorInt" "Vector"
builtin getStringElement 2 "getStringElement" "Vector"
builtin sizeOfString 1 "sizeOfString" "Vector"
builtin builtinNewString 1 "NewString" "Vector"
builtin builtinSetStringIndexInt 3 "SetStringIndex" "Vector"

list_from_vector v = go 0 (vector_size v) where
    go _ 0 = []
    go i s = get_vector_index v i:go (i+1) (s-1)

listFromString' v s i = if (i<s) then (getStringElement v i):listFromString' v s (i+1) else []

listFromString v = listFromString' v (sizeOfString v) 0

