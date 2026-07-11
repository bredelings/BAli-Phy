{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import qualified Data.Vector as V
import System.IO (print)

main = do
    let values = V.generate 5 (\index -> index * index)
    print (V.toList values)
    print (V.toList (V.map (\value -> value + 1) values))
    print (V.toList (V.imap (\index value -> index + value) values))
