{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where

import Compiler.Num
import Data.Ord

data EVector = EVector

builtin builtin_set_vector_index 3 "set_vector_index" "Vector"
builtin builtin_new_vector 1 "new_vector" "Vector"

builtin get_vector_index 2 "get_vector_index" "Vector"
builtin vector_size 1 "vector_size" "Vector"

-- list_to_vector:: [a] -> EVector
builtin list_to_vector 1 "list_to_vector" "Vector"

list_from_vector_of_size v n = go 0 n where
    go _ 0 = []
    go i s = get_vector_index v i:go (i+1) (s-1)

list_from_vector v = list_from_vector_of_size v (vector_size v)
