{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Fix where

import Control.Monad

--Laws:
--
-- Purity
--     mfix (return . h) = return (fix h)
-- Left shrinking (or Tightening)
--     mfix (\x -> a >>= \y -> f x y) = a >>= \y -> mfix (\x -> f x y)
-- Sliding
--     mfix (liftM h . f) = liftM h (mfix (f . h)), for strict h.
-- Nesting
--     mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)

class Monad m => MonadFix m where
    mfix :: (a -> m a) -> m a 

fix :: (a -> a) -> a
fix f = f (fix f)
