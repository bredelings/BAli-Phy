{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.IO where

import Compiler.Base
import Data.Tuple     -- for snd

import Data.Functor
import Control.Applicative
import Control.Monad

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
data IO a = IO (RealWorld -> (RealWorld,a)) |
            IOAction  (RealWorld->(RealWorld,a)) |
            IOChangeable (IO a) |
            IOLazy (IO a) |
            IOMFix (a -> IO a) |
            IOReturn a |
            forall b. IOAndPass (IO b) (b -> IO a)

instance Functor IO where
    fmap f x = IOAndPass x (\result -> IOReturn (f result))

instance Applicative IO where
    pure x  = IO (\s -> (s,x))
    f <*> x = IOAndPass x (\x' -> IOAndPass f (\f' -> pure (f' x')))

instance Monad IO where
    f >>= g  = IOAndPass f g
    mfix f   = IOMFix f
    unsafeInterleaveIO f = IOLazy f


unsafePerformIO :: IO c -> c
unsafePerformIO (IO f) = snd (f 0#)
unsafePerformIO (IOAction f) = snd (f 0#)
unsafePerformIO (IOChangeable f) = _changeable_apply unsafePerformIO f
unsafePerformIO (IOLazy f) = unsafePerformIO f
unsafePerformIO (IOAndPass (IOLazy f) g) = let x = unsafePerformIO f in unsafePerformIO (g x)
unsafePerformIO (IOAndPass (IO f) g) = case f 0# of (s,x) -> s `seq` unsafePerformIO (g x)
unsafePerformIO (IOAndPass f g) = let x = unsafePerformIO f in x `seq` unsafePerformIO (g x)
unsafePerformIO (IOMFix f) = let x = unsafePerformIO (f x) in x
unsafePerformIO (IOReturn x) = x

foreign import bpcall "Modifiables:changeable_apply" _changeable_apply :: (a -> b) -> a -> b
changeableIO f = IOChangeable f

makeIO f = IO (\s -> let x = f s in (x `seq` 0#, x))
