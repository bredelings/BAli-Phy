{-# LANGUAGE NoImplicitPrelude #-}
module Main where

type family HiddenResult a

foreign import ecall "Prelude:div_int"
    hiddenResult :: Int -> HiddenResult Int
