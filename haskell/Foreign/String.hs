{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.String where

import Foreign.CList

data CPPString

getStringElement :: CPPString -> Int -> Char
builtin getStringElement 2 "getStringElement" "Vector"

sizeOfString :: CPPString -> Int
builtin sizeOfString 1 "sizeOfString" "Vector"

listFromString s = unpack_cpp_string s

unpack_cpp_string :: CPPString -> [Char]
unpack_cpp_string string = map_from 0# (sizeOfString string) (\i -> getStringElement string i)

clist_to_string :: CList Char -> CPPString
builtin clist_to_string 1 "clist_to_string" "Vector"

list_to_string :: [Char] -> CPPString
list_to_string x  = clist_to_string (list_to_CList x)

pack_cpp_string = list_to_string
