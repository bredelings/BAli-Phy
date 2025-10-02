{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.IO (module Compiler.IO, module Control.Monad) where

import Compiler.Base
import Data.Tuple     -- for snd

import Data.Function  -- for (.)
import Data.Functor
import Control.Applicative
import Control.Monad

type RealWorld = Int

-- We effectively have (!RealWorld, a).  Currently we are doing this manually.
-- We can't do         (!RealWorld, !a), as this would undermine unsafeInterleaveIO.


data IO a = IO { runIO :: RealWorld -> (RealWorld,a) }

instance Functor IO where
    fmap f x = IO (\state1 -> let (state2,   result) = runIO x state1
                              in  state2 `seq` (state2, f result))

instance Applicative IO where
    pure x  = IO (\s -> (s,x))
    t1 <*> t2 = IO (\state1 -> let (state2,f) = runIO t1 state1
                                   (state3,x) = runIO t2 state2
                               in state3 `seq` (state3, f x))

instance Monad IO where
    f >>= g  = IO (\state1 -> case runIO f state1 of (state2,x) -> runIO (g x) state2)
    unsafeInterleaveIO f = IO (\s -> s `seq` (s, snd (runIO f s)) )

instance MonadFail IO where
    fail s = error s

fixIO f   = IO (\state1 -> let result@(state2,x) = runIO (f x) state1 in result)

{-# NOINLINE unsafePerformIO #-}
unsafePerformIO :: IO c -> c
unsafePerformIO f = let (s,x) = runIO f 0#
                    in x

foreign import bpcall "Modifiables:changeable_apply" _changeable_apply :: (a -> b) -> a -> b
changeableIO f = IO (\s -> _changeable_apply (runIO f) s)

-- We used to have let x = s `seq` f s in (x `seq` s, x).
-- * (s `seq` f s) emulates forcing s, so that the C++ code doesn't have to do so.
-- * (x `seq` s)   ensures that getting the new state forces f to run.

-- However, the new form allows merging the seqs, and allows eliminating seqs else where.
-- It also avoids doing (s `seq` x `seq` s) for the state.

makeIO f = IO (\s -> let x = f s in x `seq` s `seq` (s, x))

lazySequence :: Functor f => f (IO a) -> IO (f a)
lazySequence obj = return $ fmap unsafePerformIO obj

