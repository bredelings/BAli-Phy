{-# LANGUAGE NoImplicitPrelude #-}
module AssocShadowClassSupport (C(..)) where

class C a where
    type T a
