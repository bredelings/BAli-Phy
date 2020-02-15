{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.IO (unsafeInterleaveIO,
                    unsafePerformIO)
    where

import Compiler.Base -- for seq, IO = IOActionX, LazyIO, IOAndPass, MFIX, IOReturn
import Data.Tuple    -- for snd

unsafeInterleaveIO x = LazyIO x

unsafePerformIO (IOAction f) = snd (f 0)
unsafePerformIO (LazyIO f) = unsafePerformIO f
unsafePerformIO (IOAndPass (LazyIO f) g) = let x = unsafePerformIO f in unsafePerformIO (g x)
unsafePerformIO (IOAndPass f g) = let x = unsafePerformIO f in x `seq` unsafePerformIO (g x)
unsafePerformIO (MFix f) = let x = unsafePerformIO (f x) in x
unsafePerformIO (IOReturn x) = x

