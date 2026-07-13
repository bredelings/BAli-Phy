{-# LANGUAGE NoImplicitPrelude #-}
module Main where

type family Loop a where
    Loop a = Loop a

foreign import ecall "Prelude:div_int"
    loopBuiltin :: Loop Int
