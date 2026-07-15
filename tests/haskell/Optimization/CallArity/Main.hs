{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import System.IO (print)

{-# NOINLINE bump #-}
bump :: Int -> Int
bump x = x + 1

run :: Int -> Int
run x =
  let saturated a =
        case bump a of
          value -> \b -> value + b
      shared a =
        case bump a of
          value -> \b -> value + b
      partial = shared x
  in saturated x 1 + partial 2 + partial 3

main = print (run 10)
