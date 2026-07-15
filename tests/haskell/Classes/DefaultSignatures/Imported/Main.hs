{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import DefaultClass

data Box = Box

instance Identity Box

box :: Box
box = convert Box
