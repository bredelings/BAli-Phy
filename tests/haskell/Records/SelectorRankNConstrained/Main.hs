{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}

module Main where

import Compiler.Base (String)
import Compiler.Num
import System.IO (print)
import Text.Show (Show, show)

data R = R { render :: forall a. Show a => a -> String }

mkR :: R
mkR = R { render = show }

main = print (render mkR 7)
