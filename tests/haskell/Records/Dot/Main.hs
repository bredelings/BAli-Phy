{-# LANGUAGE OverloadedRecordDot #-}

module Main where

data Point = Point { x :: Int, y :: Int }

main = print ((Point 3 4).x, (.y) (Point 3 4))
