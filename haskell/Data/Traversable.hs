{-# LANGUAGE NoImplicitPrelude #-}
module Data.Traversable where

import Data.Functor
import Data.Foldable
import Control.Monad
import qualified Data.List as List (foldr)
import Data.Maybe (Maybe(..))
import Data.Array
import Control.Applicative
import Data.Function    

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

    sequenceA :: Applicative f => t (f a) -> f (t a)

    mapM :: Monad m => (a -> m b) -> t a -> m (t b)

    sequence :: Monad m => t (m a) -> m (t a)

    traverse f = sequenceA . fmap f

    sequenceA = traverse id

    mapM = traverse

    sequence = sequenceA

-- for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)

-- forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)

-- mapAccumL :: forall t s a b. Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)

-- mapAccumR :: forall t s a b. Traversable t => (s -> a -> (s, b)) -> s -> t a -> (s, t b)


instance Traversable Maybe where
    traverse _ Nothing = pure Nothing
    traverse f (Just x) = Just <$> f x

instance Traversable [] where
    traverse f = List.foldr cons' (pure [])
        where cons' x ys = liftA2 (:) (f x) ys

instance Traversable ((,) a) where
    traverse f (x,y) = (,) x <$> f y

--instance Ix i => Traversable (Array i) where
--    traverse f arr = listArray (bounds arr) `fmap` traverse f (elems arr)

instance Traversable (Array Int) where
    traverse f arr = listArray (length arr) `fmap` traverse f (elems arr)
