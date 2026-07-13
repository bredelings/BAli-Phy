{-# LANGUAGE NoImplicitPrelude #-}
module ImportedABI where

type family ImportedABI a where
    ImportedABI Int = Int -> Int -> Int
