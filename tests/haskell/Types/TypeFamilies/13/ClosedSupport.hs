{-# LANGUAGE NoImplicitPrelude #-}
module ClosedSupport where

type family Closed a where
    Closed Int = Double
