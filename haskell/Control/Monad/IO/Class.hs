module Control.Monad.IO.Class where

import Control.Monad
import Compiler.IO

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO x = x

