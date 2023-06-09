{-# LANGUAGE NoImplicitPrelude #-}
module Data.Typeable where

data TypeRep

class Typeable a where
    typeOf :: a -> TypeRep
--    typeRep :: forall proxy. proxy a -> TypeRep

{-
cast :: forall a b. (Typeable a, Typeable b) => a -> Maybe b
-}

data Proxy t
