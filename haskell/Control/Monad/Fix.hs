{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.Fix (
        MonadFix(mfix),
        fix
   ) where

import Control.Monad
import Data.Function (fix,(.))
import Data.Maybe
import Compiler.Error (error)
import Data.OldList (head, tail)
import Compiler.IO
import Compiler.ST

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

instance MonadFix [] where
    mfix f = case fix (f . head) of
               []    -> []
               (x:_) -> x : mfix (tail . f)


instance MonadFix Maybe where
    mfix f = let a = f (unJust a) in a
             where unJust (Just x) = x
                   unJust Nothing  = error "mfix Maybe: Nothing"

instance MonadFix IO where
    mfix = fixIO

instance MonadFix (ST s) where
    mfix = fixST
