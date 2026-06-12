{-# LANGUAGE NoImplicitPrelude #-}

module ForeignRecord (Foreign(Foreign), makeForeign, readForeign) where

import Compiler.Num

data Foreign = Foreign { x :: Int }

makeForeign n = Foreign n

readForeign (Foreign n) = n
