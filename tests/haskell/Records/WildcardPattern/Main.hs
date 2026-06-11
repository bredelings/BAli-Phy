{-# LANGUAGE NoImplicitPrelude #-}

module Main where

data Point = Point { x :: Int, y :: Int }

f (Point { .. }) = x
