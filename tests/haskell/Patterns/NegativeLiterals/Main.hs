{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional
import Compiler.Num hiding (negate)
import Data.Bool
import System.IO (putStrLn)

infixr 9 :^
data Chain = End | Int :^ Chain

negate _ = 2

negativeInt :: Int -> Bool
negativeInt (-1) = True
negativeInt _ = False

negativeDouble :: Double -> Bool
negativeDouble (-1.5) = True
negativeDouble _ = False

negativeHead :: Chain -> Bool
negativeHead (-1 :^ End) = True
negativeHead _ = False

report True = putStrLn "True"
report False = putStrLn "False"

main = do
  report (negativeInt (-1))
  report (negativeInt 2)
  report (negativeDouble (-1.5))
  report (negativeDouble 1.5)
  report (negativeHead (-1 :^ End))
  report (negativeHead (1 :^ End))
