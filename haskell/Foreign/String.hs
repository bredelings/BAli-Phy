{-# LANGUAGE NoImplicitPrelude #-}
module Foreign.String where

import Compiler.FFI.Runtime (RuntimeValue)
import Data.Bool
import Foreign.CList
import Foreign.Pair

-- Opaque C++ std::string transport across the builtin boundary.  Callers give
-- it text, byte, path, or diagnostic semantics at the wrapper layer.
data CPPString

instance RuntimeValue CPPString

foreign import ecall "Vector:getStringElement" getStringElement :: CPPString -> Int -> Char

foreign import ecall "Vector:sizeOfString" sizeOfString :: CPPString -> Int

foreign import ecall "Vector:decodeUtf8CharAt" decodeUtf8CharAt :: CPPString -> Int -> EPair Char Int

listFromString s = unpackUtf8String s

-- Compatibility/raw-byte helper.  Use unpackUtf8String for text decoding; this
-- remains for byte-indexed builtin plumbing that has not been split out yet.
unpack_cpp_substring string offset length = mapFrom offset length (\i -> getStringElement string i)

foreign import bpcall "Vector:" cppSubString :: CPPString -> Int -> Int -> CPPString

-- Decode a CPPString by byte offset without allocating all Chars up front.
-- decodeUtf8CharAt returns the next byte offset after each decoded scalar.
unpackUtf8Substring string offset length =
    case equals_int offset length of
      True -> []
      _ -> let char_next = decodeUtf8CharAt string offset
               c = c_fst char_next
               next = c_snd char_next
           in c : unpackUtf8Substring string next length

unpackUtf8String :: CPPString -> [Char]
unpackUtf8String string = unpackUtf8Substring string 0# (sizeOfString string)

-- Compatibility alias.  The desugarer still constructs this magic name for
-- string literals; delete the alias after that lowering is renamed.
unpack_cpp_string :: CPPString -> [Char]
unpack_cpp_string = unpackUtf8String

foreign import bpcall "Vector:" list_to_string :: [Char] -> CPPString

pack_cpp_string = list_to_string

-- Possibly this should just be "empty", but then we'd have to import this qualified and use something like FS.empty
empty_cpp_string = list_to_string ""

foreign import ecall "Prelude:" show_int :: Int -> CPPString
foreign import ecall "Prelude:" show_integer :: Integer -> CPPString
foreign import ecall "Prelude:" show_double :: Double -> CPPString
