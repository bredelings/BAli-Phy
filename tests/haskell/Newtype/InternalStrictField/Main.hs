module Main where

newtype InternalStrict = InternalStrict (Maybe !Int)

main = print 1
