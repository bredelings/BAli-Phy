module Main where

import System.IO (print)

-- The spaces around '=' on the next line are U+00A0 NO-BREAK SPACE.
nbsp = 1 :: Int

main = print (nbsp == 1)
