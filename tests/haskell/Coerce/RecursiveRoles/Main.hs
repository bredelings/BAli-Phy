{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Compiler.Prim (coerce)
import Data.Eq
import System.IO (print)

newtype Age = Age Int
data Marker = Marker
data List a = Nil | Cons a (List a)
data PhantomChain a = PDone | PNext (PhantomChain a)
data LeftChain a = LDone | LNext (RightChain a)
data RightChain a = RValue a (LeftChain a)

agesToInts :: List Age -> List Int
agesToInts = coerce

phantomAgeToMarker :: PhantomChain Age -> PhantomChain Marker
phantomAgeToMarker = coerce

leftAgesToInts :: LeftChain Age -> LeftChain Int
leftAgesToInts = coerce

listHead (Cons x _) = x
phantomDepth PDone = 0
phantomDepth (PNext xs) = 1 + phantomDepth xs
leftValue (LNext (RValue x _)) = x

ok = listHead (agesToInts (Cons (Age 3) Nil)) == 3
  && phantomDepth (phantomAgeToMarker (PNext PDone)) == 1
  && leftValue (leftAgesToInts (LNext (RValue (Age 5) LDone))) == 5

main = print (if ok then (1 :: Int) else 0)
