{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Trans.Reader
    ( Reader
    , reader
    , runReader
    , mapReader
    , withReader
    , ReaderT(..)
    , mapReaderT
    , withReaderT
    , ask
    , local
    , asks
    , liftCallCC
    , liftCatch
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Data.Function
import Data.Functor
import Data.Functor.Identity

type Reader r a = ReaderT r Identity a

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)

runReader :: Reader r a -> r -> a
runReader m = runIdentity . runReaderT m

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity)

withReader :: (r' -> r) -> Reader r a -> Reader r' a
withReader = withReaderT

-- | A computation that reads a shared environment before running in the base monad.
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT (f . runReaderT m)

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f m = ReaderT (runReaderT m . f)

instance Functor m => Functor (ReaderT r m) where
    fmap f = mapReaderT (fmap f)

instance Applicative m => Applicative (ReaderT r m) where
    pure x = ReaderT (const (pure x))
    f <*> x = ReaderT (\r -> runReaderT f r <*> runReaderT x r)
    u *> v = ReaderT (\r -> runReaderT u r *> runReaderT v r)
    u <* v = ReaderT (\r -> runReaderT u r <* runReaderT v r)
    liftA2 f x y = ReaderT (\r -> liftA2 f (runReaderT x r) (runReaderT y r))

instance Alternative m => Alternative (ReaderT r m) where
    empty = ReaderT (const empty)
    m <|> n = ReaderT (\r -> runReaderT m r <|> runReaderT n r)

instance Monad m => Monad (ReaderT r m) where
    -- Run both computations under the same immutable environment.
    m >>= k = ReaderT (\r -> do
        x <- runReaderT m r
        runReaderT (k x) r)

instance MonadFail m => MonadFail (ReaderT r m) where
    fail message = ReaderT (const (fail message))

instance MonadFix m => MonadFix (ReaderT r m) where
    mfix f = ReaderT (\r -> mfix (\x -> runReaderT (f x) r))

instance MonadTrans (ReaderT r) where
    lift m = ReaderT (const m)

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

ask :: Monad m => ReaderT r m r
ask = ReaderT return

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local = withReaderT

asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = ReaderT (return . f)

liftCallCC :: CallCC m a b -> CallCC (ReaderT r m) a b
-- Restore the environment captured where the continuation was created.
liftCallCC callCC f = ReaderT (\r ->
    callCC (\c -> runReaderT (f (ReaderT . const . c)) r))

liftCatch :: Catch e m a -> Catch e (ReaderT r m) a
-- Run an exception handler under the same environment as the protected computation.
liftCatch catch m handler = ReaderT (\r ->
    catch (runReaderT m r) (\e -> runReaderT (handler e) r))
