{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.IO (unsafeInterleaveIO,
                    unsafePerformIO)
    where

import Compiler.Base -- for seq, IO = IOActionX, LazyIO, IOAndPass, MFIX, IOReturn

unsafeInterleaveIO x = LazyIO x

unsafePerformIO (IOAction1 x y) = x y
unsafePerformIO (IOAction2 x y z) = x y z
unsafePerformIO (IOAction3 x y z w) = x y z w
unsafePerformIO (IOAction4 x y z w u) = x y z w u
unsafePerformIO (LazyIO f) = unsafePerformIO f
unsafePerformIO (IOAndPass (LazyIO f) g) = let x = unsafePerformIO f in unsafePerformIO (g x)
unsafePerformIO (IOAndPass f g) = let x = unsafePerformIO f in x `seq` unsafePerformIO (g x)
unsafePerformIO (MFix f) = let x = unsafePerformIO (f x) in x
unsafePerformIO (IOReturn x) = x

