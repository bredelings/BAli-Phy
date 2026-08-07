{-# LANGUAGE NoImplicitPrelude #-}
module Data.Functor.Identity (Identity(..)) where

import Control.Applicative
import Control.Monad
import Data.Eq
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Monoid
import Data.Ord
import Data.Semigroup
import Data.Traversable
import Text.Read
import Text.Show

-- | The identity functor and monad, used as the pure base for monad transformers.
newtype Identity a = Identity { runIdentity :: a }
    deriving (Eq, Ord)

-- Match the upstream constructor-style representation rather than record-derived syntax.
instance Show a => Show (Identity a) where
    showsPrec d (Identity x) = showParen (d > 10) $ showString "Identity " . showsPrec 11 x

instance Read a => Read (Identity a) where
    readsPrec d = readParen (d > 10) $ \s ->
        [(Identity x, rest2) | (_, rest1) <- readConstructor "Identity" s,
                               (x, rest2) <- readsPrec 11 rest1]

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
    toList (Identity x) = [x]

instance Traversable Identity where
    traverse f (Identity x) = Identity <$> f x

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
    Identity x >>= f = f x

instance Semigroup a => Semigroup (Identity a) where
    Identity x <> Identity y = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty
