{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Bool
import System.IO (print, putStrLn)

pick :: Bool -> Int -> Int
pick True x = x
pick False x = x + 1

main =
  let x = 10
      value = (\x -> x + 1) 4
  in do
    print (pick True (x + value))
    putStrLn "nonrec"
