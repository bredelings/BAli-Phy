{-# LANGUAGE NoImplicitPrelude, NoFieldSelectors #-}

module RecordSupport (Point(..)) where

import Compiler.Num

data Point = Point { x :: Int, y :: Int }
