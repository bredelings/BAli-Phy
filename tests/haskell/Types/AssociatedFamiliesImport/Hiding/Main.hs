{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import AssocHideDotDotSupport hiding (C(..))
import AssocHideExplicitSupport hiding (E(U))
import AssocHideOtherSupport (T, U)
import Compiler.Num
import Data.Eq
import System.IO (print)

data Box = Box

type instance T Box = Int
type instance U Box = Int

useT :: T Box -> Int
useT x = x + 1

useU :: U Box -> Int
useU x = x + 1

ok = useT 20 + useU 20 == 42

main = print (if ok then (1 :: Int) else 0)
