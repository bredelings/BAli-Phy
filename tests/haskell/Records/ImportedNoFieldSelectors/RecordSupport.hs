{-# LANGUAGE NoImplicitPrelude, NoFieldSelectors #-}

module RecordSupport (Point(..), readPoint) where

import Compiler.Num

data Point = Point { x :: Int, y :: Int }

readPoint (Point { x = a, y = b }) = (a, b)
