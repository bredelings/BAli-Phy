{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.ST (ST, runST)
    where

import Compiler.Base -- for seq
import Control.Monad
import Data.Tuple    -- for snd
import Data.Function -- for $

data ST s a = ST { runST :: s -> (s, a) }

instance Monad (ST s) where
    f >>= g  = ST (\state1 -> let (state2,x) = runST f state1 in x `seq` runST (g x) state2)
    return x = ST (\s -> (s, x))
    mfix f   = ST (\state1 -> let result@(state2, x) = runST (f x) state1 in result)
    unsafeInterleaveIO f = ST (\state -> (state, snd $ runST f state))
