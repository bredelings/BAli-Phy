{-# LANGUAGE GADTs, NoImplicitPrelude, NoFieldSelectors #-}

module Main where

import Compiler.Num
import System.IO (print)

data T a where
    TInt :: { value :: Int, tag :: Int } -> T Int
    TBool :: { flag :: Char } -> T Char

setTag :: T Int -> T Int
setTag t = t { tag = 4 }

readTag :: T Int -> Int
readTag (TInt { tag = n }) = n

main = print (readTag (setTag (TInt { value = 2, tag = 9 })))
