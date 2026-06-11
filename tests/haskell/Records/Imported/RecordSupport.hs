{-# LANGUAGE NoImplicitPrelude #-}

module RecordSupport (Point(..), shift) where

import Compiler.Num

data Point = Point { x :: Int, y :: Int }

shift p = p { x = x p + 1, y = y p + 1 }
