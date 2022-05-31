{-# LANGUAGE NoImplicitPrelude #-}
module Compiler.Base where

import Compiler.Error  -- for error
import Data.Function   -- for id
import Data.Tuple      -- for snd

type String = [Char]

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    fail   :: [Char] -> m a
    mfix   :: (a -> m a) -> m a
    unsafeInterleaveIO :: m a -> m a

    f >> g = f >>= (\x -> g)
    fail s = error s

infixl 1 >>, >>=

join x = x >>= id

infixr 0 $!, `seq`
f $! x = x `seq` f x

foreign import bpcall "Prelude:seq" seq :: a -> b -> b

foreign import bpcall "Prelude:struct_seq" struct_seq :: a -> b -> b

data IO a = IOAction { runIO :: Int -> (Int, a) }

-- unsafePerformIO (IOAction f) = snd (f 0)
unsafePerformIO :: IO a -> a
unsafePerformIO f = snd $ runIO f 0#

instance Monad IO where
-- unsafePerformIO (IOAndPass f g) = let x = unsafePerformIO f in x `seq` unsafePerformIO (g x)
    f >>= g = IOAction (\state1 -> let (state2,x) = runIO f state1 in x `seq` runIO (g x) state2)
    return x = IOAction (\s -> (s, x))
--unsafePerformIO (MFix f) = let x = unsafePerformIO (f x) in x
    mfix f = IOAction (\state1 -> let result@(state2, x) = runIO (f x) state1 in result)
-- unsafeInterleaveIO x = LazyIO x
-- unsafePerformIO (LazyIO f) = unsafePerformIO f
-- unsafePerformIO (IOAndPass (LazyIO f) g) = let x = unsafePerformIO f in unsafePerformIO (g x)
    unsafeInterleaveIO f = IOAction (\state -> (state, snd $ runIO f state))

