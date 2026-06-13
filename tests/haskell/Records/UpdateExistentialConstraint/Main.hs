{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}

module Main where

import Compiler.Num
import System.IO (print)

class C a where
    get :: a -> Int

instance C Char where
    get _ = 3

data Box = forall a. C a => Box { payload :: a, tag :: Int }

setTag :: Box -> Box
setTag b = b { tag = 5 }

getTag :: Box -> Int
getTag (Box { tag = n }) = n

main = print (getTag (setTag (Box 'x' 2)))
