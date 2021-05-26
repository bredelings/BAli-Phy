{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad.State where

import Compiler.Base -- for IO = IOAction, LazyIO, IOAndPass, MFix, IOReturn
import Data.Tuple    -- for fst, snd
import Data.Function -- for id

-- drat -- my implementation of IO assumes that the state is FIRST, but should be SECOND.

-- data State s a = State {runState::(s->(a,s))}

runState :: s -> (a,s)
runState s (IOReturn x)             = (x,s)
runState s (IOAction f)             = let (s', x) = f s in (x,s')
-- maybe we need the msplit operation to generically do interleaved stuff in the State monad?
runState s (LazyIO f)               = runState s f
runState s (IOAndPass (LazyIO f) g) = let (x , _) = runState s f in runState s  (g x)
runState s (IOAndPass f          g) = let (x, s') = runState s f in runState s' (g x)
runState s (MFix f)                 = let xs@(x, s') = runState s (f x) in xs -- is this right?

get = IOAction id
put s = IOAction (\_ -> (s,()))
modify f = IOAction (\s -> (f s,()))

evalState x s = fst $ runState s x
execState x s = snd $ runState s x
-- mapState
