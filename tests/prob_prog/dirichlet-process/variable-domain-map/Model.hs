{-# LANGUAGE NoImplicitPrelude #-}
module Model where

import Compiler.Fractional ((/))
import Compiler.Num
import Control.Monad (return)
import Data.Function (($))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (replicate, sum)
import Probability

-- Vary the integer-key domain while a likelihood forces every map-indexed DP value.
model = do
  domainChoice <- prior $ categorical (replicate 4 (1 / 4))
  let keyList :: [Int]
      keyList = case domainChoice of
                  0 -> []
                  1 -> [3, 1]
                  2 -> [1, 4]
                  _ -> [0, 1, 3]
      keys = IS.fromList keyList
  values <- dirichletProcessMap keys 1 (normal 0 1)
  observe 0 $ normal (sum (IM.elems values)) 1
  return ["choice" %=% domainChoice, "keys" %=% keyList, "values" %=% values]

main _ = return model
