{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.String where

import Foreign.CList

data CPPString

getStringElement :: CPPString -> Int -> Char
foreign import bpcall "Vector:getStringElement" getStringElement 2

sizeOfString :: CPPString -> Int
foreign import bpcall "Vector:sizeOfString" sizeOfString 1

listFromString s = unpack_cpp_string s

unpack_cpp_string :: CPPString -> [Char]
unpack_cpp_string string = map_from 0# (sizeOfString string) (\i -> getStringElement string i)

clist_to_string :: CList Char -> CPPString
foreign import bpcall "Vector:clist_to_string" clist_to_string 1

list_to_string :: [Char] -> CPPString
list_to_string x  = clist_to_string (list_to_CList x)

pack_cpp_string = list_to_string
