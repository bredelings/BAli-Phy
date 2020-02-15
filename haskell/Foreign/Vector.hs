{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where

import Compiler.Base
import Compiler.Num
import Data.Ord
import Data.Array

data EVector = EVector

builtin get_vector_index 2 "get_vector_index" "Vector"
builtin vector_size 1 "vector_size" "Vector"

-- list_to_vector:: [a] -> EVector
builtin builtin_list_to_vector 1 "list_to_vector" "Vector"

builtin fromVectors 1 "fromVectors" "Vector"

list_from_vector_of_size v n = go 0 n where
    go _ 0 = []
    go i s = get_vector_index v i:go (i+1) (s-1)

list_from_vector v = list_from_vector_of_size v (vector_size v)

list_to_vector x = builtin_list_to_vector (deep_eval_list x)
array_to_vector x = list_to_vector (elems x)

pack_cpp_string = list_to_string
