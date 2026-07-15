{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Bool
import System.IO (print)

{-# NOINLINE letFunction #-}
letFunction :: Int -> Int -> Int
letFunction =
  let add = \x y -> x + y
  in add

{-# NOINLINE caseFunction #-}
caseFunction :: Bool -> Int -> Int
caseFunction flag =
  case flag of
    True -> \x -> x
    False -> \x -> x + 1

main = do
  print (letFunction 2 3)
  print (caseFunction False 6)
