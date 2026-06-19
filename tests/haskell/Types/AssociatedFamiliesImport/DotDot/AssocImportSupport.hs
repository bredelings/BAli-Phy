{-# LANGUAGE NoImplicitPrelude #-}
module AssocImportSupport (C(..)) where

class C a where
    type T a
    data D a
