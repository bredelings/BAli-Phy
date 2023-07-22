{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.String where

import Foreign.CList

data CPPString

foreign import bpcall "Vector:getStringElement" getStringElement :: CPPString -> Int -> Char

foreign import bpcall "Vector:sizeOfString" sizeOfString :: CPPString -> Int

listFromString s = unpack_cpp_string s

unpack_cpp_string :: CPPString -> [Char]
unpack_cpp_string string = map_from 0# (sizeOfString string) (\i -> getStringElement string i)

foreign import bpcall "Vector:clist_to_string" clist_to_string :: CList Char -> CPPString

list_to_string :: [Char] -> CPPString
list_to_string x  = clist_to_string (list_to_CList x)

pack_cpp_string = list_to_string

-- Possibly this should just be "empty", but then we'd have to import this qualified and use something like FS.empty
empty_cpp_string = list_to_string ""
