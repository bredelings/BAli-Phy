{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.State where

import Compiler.Base -- for IO = IOAction, LazyIO, IOAndPass, MFix, IOReturn
import Data.Tuple    -- for fst, snd
import Data.Function -- for id

-- drat -- my implementation of IO assumes that the state is FIRST, but should be SECOND.

-- data State s a = State {runState::(s->(a,s))}
data State s a = State (s->(a,s))

runState :: State s a -> s -> (a,s)
runState (IOReturn x) s             = (x,s)
runState (State f) s                = f s
runState (IOAction f) s             = let (s', x) = f s in (x,s')
-- maybe we need the msplit operation to generically do interleaved stuff in the State monad?
runState (LazyIO f) s               = runState f s
runState (IOAndPass (LazyIO f) g) s = let (x , _) = runState f s in runState (g x) s
runState (IOAndPass f          g) s = let (x, s') = runState f s in runState (g x) s'
runState (MFix f) s                 = let xs@(x, s') = runState (f x) s in xs -- is this right?

get      = State ( \s -> (s , s)   )
put s    = State ( \_ -> ((), s)   )
modify f = State ( \s -> ((), f s) )

evalState x s = fst $ runState x s
execState x s = snd $ runState x s
-- mapState
