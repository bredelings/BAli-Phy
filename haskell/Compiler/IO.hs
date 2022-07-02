{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.IO where

import Compiler.Base
import Control.Monad
import Data.Tuple     -- for snd

type RealWorld = Int

{-
data IO a = IOAction { runIO :: RealWorld -> (RealWorld, a) }

-- unsafePerformIO (IOAction f) = snd (f 0)
unsafePerformIO :: IO a -> a
unsafePerformIO f = snd $ runIO f 0#

instance Monad IO where
    f >>= g = IOAction (\state1 -> let (state2,x) = runIO f state1 in runIO (g x) state2)
    return x = IOAction (\s -> (s, x))
    mfix f = IOAction (\state1 -> let result@(state2, x) = runIO (f x) state1 in result)
    unsafeInterleaveIO f = IOAction (\state -> (state, snd $ runIO f state))

-}
data IO a = IOAction  (RealWorld->(RealWorld,a)) |
            IOLazy (IO a) |
            IOMFix (a -> IO a) |
            IOReturn a |
            forall b. IOAndPass (IO b) (b -> IO a)

instance Monad IO where
    f >>= g  = IOAndPass f g
    return x = IOReturn x
    mfix f   = IOMFix f
    unsafeInterleaveIO f = IOLazy f

unsafePerformIO (IOAction f) = snd (f 0#)
unsafePerformIO (IOLazy f) = unsafePerformIO f
unsafePerformIO (IOAndPass (IOLazy f) g) = let x = unsafePerformIO f in unsafePerformIO (g x)
unsafePerformIO (IOAndPass f g) = let x = unsafePerformIO f in x `seq` unsafePerformIO (g x)
unsafePerformIO (IOMFix f) = let x = unsafePerformIO (f x) in x
unsafePerformIO (IOReturn x) = x
