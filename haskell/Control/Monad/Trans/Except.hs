{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Trans.Except
    ( Except
    , except
    , runExcept
    , mapExcept
    , withExcept
    , ExceptT(ExceptT)
    , runExceptT
    , mapExceptT
    , withExceptT
    , throwE
    , catchE
    , handleE
    , tryE
    , finallyE
    , liftCallCC
    , liftListen
    , liftPass
    ) where

import Compiler.Error
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Data.Either
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Monoid

type Except e a = ExceptT e Identity a

except :: Monad m => Either e a -> ExceptT e m a
except value = ExceptT (return value)

runExcept :: Except e a -> Either e a
runExcept (ExceptT value) = runIdentity value

mapExcept :: (Either e a -> Either e' b) -> Except e a -> Except e' b
mapExcept f = mapExceptT (Identity . f . runIdentity)

withExcept :: (e -> e') -> Except e a -> Except e' a
withExcept = withExceptT

-- | A base-monad computation that returns either an exception value or a result.
newtype ExceptT e m a = ExceptT (m (Either e a))

runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT (ExceptT value) = value

mapExceptT :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f = ExceptT . f . runExceptT

withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = mapExceptT (fmap map_error) where
    map_error (Left e) = Left (f e)
    map_error (Right x) = Right x

instance Functor m => Functor (ExceptT e m) where
    fmap f = mapExceptT (fmap map_result) where
        map_result (Left e) = Left e
        map_result (Right x) = Right (f x)

instance Monad m => Applicative (ExceptT e m) where
    pure x = ExceptT (return (Right x))
    -- Stop at the first error, without running later effects.
    ExceptT mf <*> ExceptT mx = ExceptT (do
        result_f <- mf
        case result_f of
            Left e -> return (Left e)
            Right f -> do
                result_x <- mx
                case result_x of
                    Left e -> return (Left e)
                    Right x -> return (Right (f x)))

instance (Monad m, Monoid e) => Alternative (ExceptT e m) where
    empty = ExceptT (return (Left mempty))
    -- Try the second computation after failure and combine errors if both fail.
    ExceptT mx <|> ExceptT my = ExceptT (do
        result_x <- mx
        case result_x of
            Right x -> return (Right x)
            Left e1 -> do
                result_y <- my
                case result_y of
                    Right y -> return (Right y)
                    Left e2 -> return (Left (mappend e1 e2)))

instance Monad m => Monad (ExceptT e m) where
    -- Propagate an error without invoking the continuation.
    m >>= k = ExceptT (do
        result <- runExceptT m
        case result of
            Left e -> return (Left e)
            Right x -> runExceptT (k x))

instance MonadFail m => MonadFail (ExceptT e m) where
    fail = ExceptT . fail

instance MonadFix m => MonadFix (ExceptT e m) where
    mfix f = ExceptT (mfix (runExceptT . f . either (const failed) id)) where
        failed = error "mfix (ExceptT): inner computation returned Left value"

instance MonadTrans (ExceptT e) where
    lift = ExceptT . liftM Right

instance MonadIO m => MonadIO (ExceptT e m) where
    liftIO = lift . liftIO

throwE :: Monad m => e -> ExceptT e m a
throwE = ExceptT . return . Left

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
-- Replace a failed computation with the handler while leaving successes unchanged.
catchE m handler = ExceptT (do
    result <- runExceptT m
    case result of
        Left e -> runExceptT (handler e)
        Right x -> return (Right x))

handleE :: Monad m => (e -> ExceptT e' m a) -> ExceptT e m a -> ExceptT e' m a
handleE = flip catchE

tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE m = catchE (liftM Right m) (return . Left)

finallyE :: Monad m => ExceptT e m a -> ExceptT e m () -> ExceptT e m a
-- Run the closer after either result, with a closer failure taking precedence.
finallyE m closer = do
    result <- tryE m
    closer
    either throwE return result

liftCallCC :: CallCC m (Either e a) (Either e b) -> CallCC (ExceptT e m) a b
-- Encode a continuation result as a successful ExceptT result.
liftCallCC callCC f = ExceptT (callCC (\c ->
    runExceptT (f (\x -> ExceptT (c (Right x))))))

liftListen :: Monad m => Listen w m (Either e a) -> Listen w (ExceptT e m) a
-- Attach the listened output only to successful results.
liftListen listen = mapExceptT (\m -> do
    (result, output) <- listen m
    return (case result of
        Left e -> Left e
        Right x -> Right (x, output)))

liftPass :: Monad m => Pass w m (Either e a) -> Pass w (ExceptT e m) a
-- Preserve output after failure and apply the supplied output function after success.
liftPass pass = mapExceptT (\m -> pass (do
    result <- m
    return (case result of
        Left e -> (Left e, id)
        Right (x, f) -> (Right x, f))))
