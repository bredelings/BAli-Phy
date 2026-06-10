{-# LANGUAGE GADTs #-}
module Main where

newtype StrictGADT where
  StrictGADT :: !Int -> StrictGADT

main = print 1
