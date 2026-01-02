{-# LANGUAGE NoImplicitPrelude #-}
module Text.Read where

import Foreign.String
import Data.List
import Data.Function
import Compiler.Error

class Read a where
    read :: [Char] -> a

instance Read Int where
    read [] = error "Can't convert empty string to int."
    read s = read_int (list_to_string s)

instance Read Double where
    read [] = error "Can't convert empty string to double."
    read s = read_double (list_to_string s)

foreign import ecall "Prelude:" read_int :: CPPString -> Int
foreign import ecall "Prelude:" read_double :: CPPString -> Double

