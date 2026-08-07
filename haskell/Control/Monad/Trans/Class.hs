{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Trans.Class (MonadTrans(..)) where

import Control.Monad

-- | A monad transformer embeds computations from an underlying monad.
class MonadTrans t where
    lift :: Monad m => m a -> t m a
