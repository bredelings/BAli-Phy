{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.ST (ST, runST, fixST)
    where

import Compiler.Base -- for seq
import Data.Tuple    -- for snd
import Data.Function -- for $

import Data.Functor
import Control.Applicative
import Control.Monad

data ST s a = ST { runST :: s -> (s, a) }

instance Functor (ST s) where
    fmap f t = ST (\state1 -> let (state2,   result) = runST t state1
                              in  (state2, f result)
                  )

instance Applicative (ST s) where
    pure x    = ST (\s -> (s, x))
    t1 <*> t2 = ST (\state1 -> let (state2,f) = runST t1 state1
                                   (state3,x) = runST t2 state2
                               in (state3, f x))

instance Monad (ST s) where
    f >>= g  = ST (\state1 -> let (state2,x) = runST f state1 in x `seq` runST (g x) state2)
    unsafeInterleaveIO f = ST (\state -> (state, snd $ runST f state))

fixST f   = ST (\state1 -> let result@(state2, x) = runST (f x) state1 in result)
