{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

data Box = forall a. Box { payload :: a, tag :: Int }

setTag :: Box -> Box
setTag b = b { tag = 5 }

getTag :: Box -> Int
getTag (Box { tag = n }) = n

main = print (getTag (setTag (Box 'x' 2)))
