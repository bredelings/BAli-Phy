{-# LANGUAGE NoImplicitPrelude #-}
module AssocBaseSupport (C(..)) where

class C a where
    type T a
    data D a
