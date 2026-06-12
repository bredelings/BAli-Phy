{-# LANGUAGE NoImplicitPrelude #-}

module LocalRecord (Local(..)) where

import Compiler.Num

data Local = Local { x :: Int }
