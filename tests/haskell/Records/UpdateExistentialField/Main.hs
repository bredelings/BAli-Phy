{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Box = forall a. Box { payload :: a, tag :: Int }

setPayload :: Box -> Box
setPayload b = b { payload = 5 }

getTag :: Box -> Int
getTag (Box { tag = n }) = n

main = print (getTag (setPayload (Box 'x' 2)))
