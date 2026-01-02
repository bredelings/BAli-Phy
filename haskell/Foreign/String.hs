{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.String where

import Foreign.CList

data CPPString

foreign import bpcall "Vector:getStringElement" getStringElement :: CPPString -> Int -> Char

foreign import bpcall "Vector:sizeOfString" sizeOfString :: CPPString -> Int

listFromString s = unpack_cpp_string s

unpack_cpp_substring string offset length = mapFrom offset length (\i -> getStringElement string i)

foreign import bpcall "Vector:" cppSubString :: CPPString -> Int -> Int -> CPPString

unpack_cpp_string :: CPPString -> [Char]
unpack_cpp_string string = unpack_cpp_substring string 0# (sizeOfString string)

foreign import bpcall "Vector:clist_to_string" clist_to_string :: CList Char -> CPPString

list_to_string :: [Char] -> CPPString
list_to_string x  = clist_to_string (listToCList x)

pack_cpp_string = list_to_string

-- Possibly this should just be "empty", but then we'd have to import this qualified and use something like FS.empty
empty_cpp_string = list_to_string ""

foreign import ecall "Prelude:" show_int :: Int -> CPPString
foreign import ecall "Prelude:" show_integer :: Integer -> CPPString
foreign import ecall "Prelude:" show_double :: Double -> CPPString

