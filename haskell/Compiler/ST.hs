{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.ST (unsafeInterleaveST,
                    runST)
    where

import Compiler.Base -- for seq, IO = IOActionX, LazyIO, IOAndPass, MFIX, IOReturn
import Data.Tuple    -- for snd

unsafeInterleaveST x = LazyIO x

runST (IOAction f) = snd (f 0)
runST (LazyIO f) = runST f
runST (IOAndPass (LazyIO f) g) = let x = runST f in runST (g x)
-- probably we can get this effect by rerunning the whole computation if either the state or result changes. using join.
runST (IOAndPass f g) = let x = runST f in x `join` runST (g x)
runST (MFix f) = let x = runST (f x) in x
runST (IOReturn x) = x
