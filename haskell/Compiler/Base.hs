{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Base where

import Compiler.Error  -- for error
import Data.Function   -- for id
import Data.Tuple      -- for snd

type String = [Char]

type RealWorld = Int

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    fail   :: String -> m a
    mfix   :: (a -> m a) -> m a
    unsafeInterleaveIO :: m a -> m a

    f >> g = f >>= (\x -> g)
    fail s = error s
    mfix = error "no mfix for this class"
    unsafeInterleaveIO = error "no unsafeInterleaveIO for this class"

infixl 1 >>, >>=

join x = x >>= id

infixr 0 $!, `seq`
f $! x = x `seq` f x

foreign import bpcall "Prelude:seq" seq :: a -> b -> b

foreign import bpcall "Prelude:struct_seq" struct_seq :: a -> b -> b
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
