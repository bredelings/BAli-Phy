{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.String where

data CPPString

getStringElement :: CPPString -> Int -> Char
builtin getStringElement 2 "getStringElement" "Vector"

sizeOfString :: CPPString -> Int
builtin sizeOfString 1 "sizeOfString" "Vector"

listFromString s = unpack_cpp_string s

-- This is an attempt to avoid importing anything.
-- If we import something, that something might already reference Foreign.String.unpack_cpp_string
increment_int :: Int -> Int
builtin increment_int 1 "increment_int" "Prelude"

subtract_int' :: Int -> Int -> Int
builtin subtract_int' 2 "subtract_int" "Prelude"

unpack_cpp_string string = go string 0# where
    size = sizeOfString string
    go string i = case subtract_int' i size of
                    0# -> []
                    _ -> getStringElement string i : go string (increment_int i)
