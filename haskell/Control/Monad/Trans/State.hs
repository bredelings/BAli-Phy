{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Trans.State
    ( State
    , state
    , runState
    , evalState
    , execState
    , mapState
    , withState
    , StateT(..)
    , evalStateT
    , execStateT
    , mapStateT
    , withStateT
    , get
    , put
    , modify
    , modify'
    , modifyM
    , gets
    , liftCallCC
    , liftCallCC'
    , liftCatch
    , liftListen
    , liftPass
    ) where

import Compiler.Base
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Tuple

-- This implements the lazy interface reexported by upstream's State module.
-- Explicit Lazy and Strict submodules remain to be ported.
type State s a = StateT s Identity a

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

runState :: State s a -> s -> (a, s)
runState m = runIdentity . runStateT m

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s
execState m s = snd (runState m s)

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f = mapStateT (Identity . f . runIdentity)

withState :: (s -> s) -> State s a -> State s a
withState = withStateT

-- | A computation that threads an updatable state through a base monad.
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT m s = do
    ~(a, _) <- runStateT m s
    return a

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT m s = do
    ~(_, s') <- runStateT m s
    return s'

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT (f . runStateT m)

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f m = StateT (runStateT m . f)

instance Functor m => Functor (StateT s m) where
    fmap f m = StateT (\s -> fmap (map_value f) (runStateT m s)) where
        map_value g ~(x, s') = (g x, s')

instance Monad m => Applicative (StateT s m) where
    pure x = StateT (\s -> return (x, s))
    StateT mf <*> StateT mx = StateT (\s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s''))

instance (Monad m, Alternative m) => Alternative (StateT s m) where
    empty = StateT (const empty)
    StateT first <|> StateT second = StateT (\s -> first s <|> second s)

instance Monad m => Monad (StateT s m) where
    m >>= k = StateT (\s -> do
        ~(x, s') <- runStateT m s
        runStateT (k x) s')

instance MonadFail m => MonadFail (StateT s m) where
    fail message = StateT (const (fail message))

instance MonadFix m => MonadFix (StateT s m) where
    -- Tie only the returned value; every recursive approximation starts from the same state.
    mfix f = StateT (\s -> mfix (\ ~(x, _) -> runStateT (f x) s))

instance MonadTrans (StateT s) where
    lift m = StateT (\s -> do
        x <- m
        return (x, s))

instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO

get :: Monad m => StateT s m s
get = state (\s -> (s, s))

put :: Monad m => s -> StateT s m ()
put s = state (const ((), s))

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state (\s -> ((), f s))

modify' :: Monad m => (s -> s) -> StateT s m ()
modify' f = do
    s <- get
    put $! f s

modifyM :: Monad m => (s -> m s) -> StateT s m ()
modifyM f = StateT (\s -> do
    s' <- f s
    return ((), s'))

gets :: Monad m => (s -> a) -> StateT s m a
gets f = state (\s -> (f s, s))

-- Restore the state captured where the continuation was created.
liftCallCC :: CallCC m (a, s) (b, s) -> CallCC (StateT s m) a b
liftCallCC callCC f = StateT (\s ->
    callCC (\c -> runStateT (f (\x -> StateT (\_ -> c (x, s)))) s))

-- Pass the state at the point where the continuation is invoked.
liftCallCC' :: CallCC m (a, s) (b, s) -> CallCC (StateT s m) a b
liftCallCC' callCC f = StateT (\s ->
    callCC (\c -> runStateT (f (\x -> StateT (\s' -> c (x, s')))) s))

-- Roll back to the state at which the protected computation began before handling an exception.
liftCatch :: Catch e m (a, s) -> Catch e (StateT s m) a
liftCatch catch m handler = StateT (\s ->
    catch (runStateT m s) (\e -> runStateT (handler e) s))

-- Preserve the final state while attaching the listened output to the returned value.
liftListen :: Monad m => Listen w m (a, s) -> Listen w (StateT s m) a
liftListen listen m = StateT (\s -> do
    ~((x, s'), output) <- listen (runStateT m s)
    return ((x, output), s'))

-- Move the output transformation outside the state/result pair expected by the base monad.
liftPass :: Monad m => Pass w m (a, s) -> Pass w (StateT s m) a
liftPass pass m = StateT (\s -> pass (do
    ~((x, transform), s') <- runStateT m s
    return ((x, s'), transform)))
