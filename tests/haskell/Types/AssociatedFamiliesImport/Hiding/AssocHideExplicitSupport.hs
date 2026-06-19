{-# LANGUAGE NoImplicitPrelude #-}
module AssocHideExplicitSupport (E(..)) where

class E a where
    type U a
