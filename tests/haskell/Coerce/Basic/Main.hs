{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Compiler.Error (error)
import Compiler.IO (IO(IO))
import Compiler.Prim (coerce)
import Data.Eq

newtype Age = Age Int
newtype Box a = Box a
data Phantom a = Phantom Int

unAge :: Age -> Int
unAge = coerce

boxAgeToInt :: Box Age -> Box Int
boxAgeToInt = coerce

phantomAgeToBool :: Phantom Age -> Phantom Bool
phantomAgeToBool = coerce

getBox (Box x) = x
getPhantom (Phantom x) = x

ok = unAge (Age 3) == 3
  && getBox (boxAgeToInt (Box (Age 4))) == 4
  && getPhantom (phantomAgeToBool (Phantom 5 :: Phantom Age)) == 5

main = if ok then IO (\s -> (s, ())) else error "coerce failed"
