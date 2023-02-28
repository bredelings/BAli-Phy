{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.IO where

import Compiler.Base
import Data.Tuple     -- for snd

import Data.Function  -- for (.)
import Data.Functor
import Control.Applicative
import Control.Monad

type RealWorld = Int

-- Hmm... so maybe we want RealWorld -> (!RealWorld,a) ?
-- Then accessing the value would force the state as well.
-- And accessing the value should force the state.

data IO a = IO { runIO :: RealWorld -> (RealWorld,a)}

instance Functor IO where
    fmap f x = IO (\state1 -> let (state2,   result) = runIO x state1
                              in  (state2, f result))

instance Applicative IO where
    pure x  = IO (\s -> (s,x))
    t1 <*> t2 = IO (\state1 -> let (state2,f) = runIO t1 state1
                                   (state3,x) = runIO t2 state2
                               in (state3, f x))

instance Monad IO where
    f >>= g  = IO (\state1 -> case runIO f state1 of (state2,x) -> state2 `seq` runIO (g x) state2)
    mfix f   = IO (\state1 -> let result@(state2,x) = runIO (f x) state1 in result)
    unsafeInterleaveIO f = IO (\s -> (s, s `seq` snd (runIO f s)) )

unsafePerformIO :: IO c -> c
unsafePerformIO f = let (s,x) = runIO f 0#
                    in s `seq` x

foreign import bpcall "Modifiables:changeable_apply" _changeable_apply :: (a -> b) -> a -> b
changeableIO f = IO (\s -> _changeable_apply (runIO f) s)

-- In (state,value), the value is not responsible for forcing the state.
-- Ideally, packaging them in the pair should force the state.
-- Right now, the caller forces the state.

-- We add an extra s2 argument to builtin_exchangeable to prevent floating outside the s2 lambda.
foreign import bpcall "Modifiables:exchangeable" builtin_exchangeable :: (a->b) -> a -> a -> b
exchangeableIO :: IO a -> IO (IO a)
-- We also ensure that performing the exchangeable action FORCES the current IO state s2, even though
-- it is not USED.
exchangeableIO f = IO $ \s1 -> (s1, makeIO $ snd . builtin_exchangeable (runIO f) s1 )
-- PROBLEM: Because the exchangeable function produces a pair, we always end up with a changeable
-- node as the list argument, instead of an exchangeable node as a list argument.
-- Therefore, builtin_function_exchange_list_entries doesn't work.

makeIO f = IO (\s -> let x = s `seq` f s  -- This emulates f forcing s, so the C++ code doesn't have to.
                     in (x `seq` s, x))   -- This ensures that getting the new state forces f to run.
                                          -- But if the pair is strict in the state, then just getting the pair
                                          -- forces f to run...
