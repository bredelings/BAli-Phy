{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Data.Eq
import System.IO (print)

data Color = Red | Blue

deriving stock instance Eq Color

class Twice a where
  twice :: a -> a

instance Twice Int where
  twice x = x + x

newtype Age = Age Int

deriving newtype instance Twice Age

unAge (Age x) = x

ok = Red == Red
  && not (Red == Blue)
  && unAge (twice (Age 21)) == 42

main = print (if ok then (1 :: Int) else 0)
