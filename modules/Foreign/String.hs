{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.String where

data CppString

builtin builtinNewString 1 "NewString" "Vector"
builtin builtinSetStringIndexInt 3 "SetStringIndex" "Vector"

builtin getStringElement 2 "getStringElement" "Vector"
builtin sizeOfString 1 "sizeOfString" "Vector"

builtin builtin_unpack_cpp_string 2 "unpack_cpp_string" "Vector"

unpack_cpp_string s = builtin_unpack_cpp_string s 0
listFromString s = unpack_cpp_string s

