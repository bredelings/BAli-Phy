{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.String where

import Compiler.Num -- for (-)

data CPPString

getStringElement :: CPPString -> Int -> Char
builtin getStringElement 2 "getStringElement" "Vector"

sizeOfString :: CPPString -> Int
builtin sizeOfString 1 "sizeOfString" "Vector"

unpack_cpp_string s = [getStringElement s i | i <- [0..sizeOfString s - 1]]
listFromString s = unpack_cpp_string s

