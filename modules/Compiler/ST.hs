{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.ST (unsafeInterleaveST,
                    runST)
    where

import Compiler.Base -- for seq, IO = IOActionX, LazyIO, IOAndPass, MFIX, IOReturn

unsafeInterleaveST x = LazyIO x

runST' (IOAction1 x y) = x y
runST' (IOAction2 x y z) = x y z
runST' (IOAction3 x y z w) = x y z w
runST' (IOAction4 x y z w u) = x y z w u
runST' (LazyIO f) = runST' f
runST' (IOAndPass (LazyIO f) g) = let x = runST' f in runST' (g x)
runST' (IOAndPass f g) = let x = runST' f in x `join` runST' (g x)
runST' (MFix f) = let x = runST' (f x) in x
runST' (IOReturn x) = x

builtin reapply 2 "reapply" "Prelude"
runST x = reapply runST' x
