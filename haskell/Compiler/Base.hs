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

data IO a = IOAction { runIO :: RealWorld -> (RealWorld, a) }

-- unsafePerformIO (IOAction f) = snd (f 0)
unsafePerformIO :: IO a -> a
unsafePerformIO f = snd $ runIO f 0#

instance Monad IO where
    -- Here we weirdly start (g x) from `state1` again, not using `state2`.
    -- We use `seq` to sequence things.
    f >>= g = IOAction (\state1 -> let (state2,x) = runIO f state1 in runIO (g x) state2)
    return x = IOAction (\s -> (s, x))
    mfix f = IOAction (\state1 -> let result@(state2, x) = runIO (f x) state1 in result)
{- unsafeInterleaveIO x = LazyIO x
   unsafePerformIO (LazyIO f) = unsafePerformIO f
   unsafePerformIO (IOAndPass (LazyIO f) g) = let x = unsafePerformIO f in unsafePerformIO (g x)
-}
    unsafeInterleaveIO f = IOAction (\state -> (state, snd $ runIO f state))

