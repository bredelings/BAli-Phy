{-# LANGUAGE NoImplicitPrelude #-}

module Main where

data Point = Point { x :: Int, y :: Int }

makePoint x y = Point { x, y }
