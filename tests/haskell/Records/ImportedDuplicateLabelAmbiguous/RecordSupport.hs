{-# LANGUAGE DuplicateRecordFields, NoImplicitPrelude #-}

module RecordSupport where

data A = A { shared :: Int, onlyA :: Int }
data B = B { shared :: Int, onlyB :: Int }
