{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where

import Compiler.Num
import Data.Ord
import Foreign.Pair

data EVector = EVector

builtin builtin_set_vector_index 3 "set_vector_index" "Vector"
builtin builtin_new_vector 1 "new_vector" "Vector"

builtin get_vector_index 2 "get_vector_index" "Vector"
builtin vector_size 1 "vector_size" "Vector"

-- list_to_vector:: [a] -> EVector
builtin builtin_list_to_vector 1 "list_to_vector" "Vector"
builtin array_to_vector 1 "array_to_vector" "Vector"

builtin fromVectors 1 "fromVectors" "Vector"

list_from_vector_of_size v n = go 0 n where
    go _ 0 = []
    go i s = get_vector_index v i:go (i+1) (s-1)

list_from_vector v = list_from_vector_of_size v (vector_size v)

deep_eval_list [] = []
deep_eval_list (x:xs) = c_pair x (deep_eval_list xs)

list_to_vector x = builtin_list_to_vector (deep_eval_list x)
