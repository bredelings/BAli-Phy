{-# LANGUAGE NoImplicitPrelude #-}
module Model where

import Compiler.Fractional ((/))
import Compiler.Num
import Control.Monad (return)
import Data.Function (($))
import Data.List (map, replicate, sum)
import Data.Tuple (snd)
import Probability

-- Vary the ordered domain while a likelihood forces every keyed DP value.
model = do
  domainChoice <- prior $ categorical (replicate 5 (1 / 5))
  let keys :: [Int]
      keys = case domainChoice of
               0 -> []
               1 -> [3, 1]
               2 -> [1, 3]
               3 -> [1, 4]
               _ -> [0, 1, 3]
  values <- dirichletProcessOn keys 1 (normal 0 1)
  observe 0 $ normal (sum (map snd values)) 1
  return ["choice" %=% domainChoice, "keys" %=% keys, "values" %=% values]

main _ = return model
