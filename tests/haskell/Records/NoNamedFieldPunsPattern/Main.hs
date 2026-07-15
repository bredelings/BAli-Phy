{-# LANGUAGE NoImplicitPrelude, NoNamedFieldPuns #-}

module Main where

data Point = Point { x :: Int, y :: Int }

getX (Point { x }) = x
