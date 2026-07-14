{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Compiler.Base (String)
import System.IO (print)

foreign import trcall "Vector:cppSubString"
    cachedSubstring :: String -> Int -> Int -> String

main = print (1 + 1 :: Int)
