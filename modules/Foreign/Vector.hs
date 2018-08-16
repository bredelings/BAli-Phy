{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.Vector where
{
import Compiler.Num;
import Data.Ord;

builtin builtin_set_vector_index 3 "set_vector_index" "Vector";
builtin builtin_new_vector 1 "new_vector" "Vector";
builtin get_vector_index 2 "get_vector_index" "Vector";
builtin vector_size 1 "vector_size" "Vector";
builtin sizeOfVectorUnsigned 1 "sizeOfVectorUnsigned" "Vector";
builtin sizeOfVectorInt 1 "sizeOfVectorInt" "Vector";
builtin sizeOfVectorVectorInt 1 "sizeOfVectorVectorInt" "Vector";
builtin sizeOfVectorvectorInt 1 "sizeOfVectorvectorInt" "Vector";
builtin getVectorIntElement 2 "getVectorIntElement" "Vector";
builtin getVectorVectorIntElement 2 "getVectorVectorIntElement" "Vector";
builtin getVectorvectorIntElement 2 "getVectorvectorIntElement" "Vector";
builtin getStringElement 2 "getStringElement" "Vector";
builtin sizeOfString 1 "sizeOfString" "Vector";
builtin builtinNewVectorInt 1 "NewVectorInt" "Vector";
builtin builtinSetVectorIndexInt 3 "SetVectorIndexInt" "Vector";
builtin builtinNewString 1 "NewString" "Vector";
builtin builtinSetStringIndexInt 3 "SetStringIndex" "Vector";
builtin builtinNewVectorDouble 1 "NewVectorDouble" "Vector";
builtin builtinSetVectorIndexDouble 3 "SetVectorIndexDouble" "Vector";
builtin builtinNewVectorMatrix 1 "NewVectorMatrix" "Vector";
builtin builtinSetVectorIndexMatrix 3 "SetVectorIndexMatrix" "Vector";

list_from_vector' v s i = if (i<s) then (get_vector_index v i):list_from_vector' v s (i+1) else [];

list_from_vector v = list_from_vector' v (vector_size v) 0;

listFromVectorInt' v s i = if (i<s) then (getVectorIntElement v i):listFromVectorInt' v s (i+1) else [];

listFromVectorInt v = listFromVectorInt' v (sizeOfVectorInt v) 0;

listFromString' v s i = if (i<s) then (getStringElement v i):listFromString' v s (i+1) else [];

listFromString v = listFromString' v (sizeOfString v) 0;

listFromVectorVectorInt' v s i = if (i<s) then (getVectorVectorIntElement v i):listFromVectorVectorInt' v s (i+1) else [];

listFromVectorVectorInt v = listFromVectorVectorInt' v (sizeOfVectorVectorInt v) 0;

listFromVectorvectorInt' v s i = if (i<s) then (getVectorvectorIntElement v i):listFromVectorvectorInt' v s (i+1) else [];

listFromVectorvectorInt v = listFromVectorvectorInt' v (sizeOfVectorvectorInt v) 0;
}
