{-# LANGUAGE NoImplicitPrelude #-}

module Compiler.IO where

import Compiler.Base
import Data.Tuple     -- for snd

import Data.Functor
import Control.Applicative
import Control.Monad

type RealWorld = Int

{-
data IO a = IO { runIO :: RealWorld -> (RealWorld, a) }

-- unsafePerformIO (IO f) = snd (f 0)
unsafePerformIO :: IO a -> a
unsafePerformIO f = snd $ runIO f 0#

instance Monad IO where
    f >>= g = IO (\state1 -> let (state2,x) = runIO f state1 in runIO (g x) state2)
    return x = IO (\s -> (s, x))
    mfix f = IO (\state1 -> let result@(state2, x) = runIO (f x) state1 in result)
    unsafeInterleaveIO f = IO (\state -> (state, snd $ runIO f state))

-}
data IO a = IO (RealWorld -> (RealWorld,a)) |
            IOAction  (RealWorld->(RealWorld,a)) |
            IOChangeable (IO a) |
            IOReturn a |
            forall b. IOAndPass (IO b) (b -> IO a)

instance Functor IO where
    fmap f x = IOAndPass x (\result -> IOReturn (f result))

instance Applicative IO where
    pure x  = IO (\s -> (s,x))
    f <*> x = IOAndPass x (\x' -> IOAndPass f (\f' -> pure (f' x')))

instance Monad IO where
    f >>= g  = IOAndPass f g
    mfix f   = IO (\state1 -> let result@(state2,x) = runIO (f x) state1 in result)
    unsafeInterleaveIO f = IO (\s -> (s, unsafePerformIO f))

runIO (IO f) s = f s
runIO g      s = let x = unsafePerformIO g in (x `seq` s, x)

unsafePerformIO :: IO c -> c
unsafePerformIO (IO f) = snd (f 0#)
unsafePerformIO (IOAction f) = snd (f 0#)
unsafePerformIO (IOChangeable f) = _changeable_apply unsafePerformIO f
unsafePerformIO (IOAndPass (IO f) g) = case f 0# of (s,x) -> s `seq` unsafePerformIO (g x)
unsafePerformIO (IOAndPass f g) = let x = unsafePerformIO f in x `seq` unsafePerformIO (g x)
unsafePerformIO (IOReturn x) = x

foreign import bpcall "Modifiables:changeable_apply" _changeable_apply :: (a -> b) -> a -> b
changeableIO f = IOChangeable f

-- Getting the value (x) forces the previous state.
-- Getting the state (x `seq` 0#) forces the  value.
makeIO f = IO (\s -> let x = s `seq` f s in (x `seq` 0#, x))

