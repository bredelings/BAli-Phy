{-# LANGUAGE NoImplicitPrelude #-}
module AssocExplicitSupport (C(T, D)) where

class C a where
    type T a
    data D a
