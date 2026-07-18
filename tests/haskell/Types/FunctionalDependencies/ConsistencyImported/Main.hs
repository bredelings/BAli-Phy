{-# LANGUAGE NoImplicitPrelude, FunctionalDependencies #-}

module Main where

import Support

instance C Int Char where
    keep x = x
