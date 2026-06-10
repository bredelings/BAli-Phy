{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Main where

import Compiler.Error (error)
import Compiler.Num
import Data.Eq
import Data.List (map)
import System.IO (print)

undefined = error "undefined"

newtype Box a where
  Box :: forall b. b -> Box b

unBox (Box x) = x

lazyMatch = case (undefined :: Box Int) of Box _ -> 7

ok = unBox (Box 5) == 5
  && map unBox (map Box [1,2]) == [1,2]
  && lazyMatch == 7

main = print (if ok then (1 :: Int) else 0)
