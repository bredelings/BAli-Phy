{-# LANGUAGE NoImplicitPrelude #-}
module Data.Traversable where

import Data.Functor
import Data.Foldable
import Control.Monad

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

    sequenceA :: Applicative f => t (f a) -> f (t a)

    mapM :: Monad m => (a -> m b) -> t a -> m (t b)

    sequence :: Monad m => t (m a) -> m (t a)

-- for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)

-- forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)

-- mapAccumL :: forall t s a b. Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)

-- mapAccumR :: forall t s a b. Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)
