{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Signatures where

type CallCC m a b = ((a -> m b) -> m a) -> m a

type Catch e m a = m a -> (e -> m a) -> m a

type Listen w m a = m a -> m (a, w)

type Pass w m a = m (a, w -> w) -> m a
