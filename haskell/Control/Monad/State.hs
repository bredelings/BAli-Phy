{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.State where

import Data.Tuple    -- for fst, snd
import Data.Function -- for id
import Control.Monad

data State s a = State { runState :: s->(a,s) }

instance Monad State where
    f >>= g = State (\s1 -> let (x,s2) = runState f s1 in runState g s2)
    return x = State (\s -> (x,s))
    mfix f = State (\state1 -> let result@(state1, x) = runState (f x) state1 in result)
    unsafeInterleaveIO f = State (\state -> (state, snd $ runState f state))

get      = State ( \s -> (s , s)   )
put s    = State ( \_ -> ((), s)   )
modify f = State ( \s -> ((), f s) )

evalState x s = fst $ runState x s
execState x s = snd $ runState x s
-- mapState
