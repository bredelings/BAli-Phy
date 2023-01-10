{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad where

import Compiler.Base
import Compiler.Error  -- for error
import Data.Function   -- for id
import Data.List
import Data.Functor
import Data.Ord
import Compiler.Num

import Control.Applicative

infixl 1 >>, >>=

class Applicative m => Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    fail   :: String -> m a
    mfix   :: (a -> m a) -> m a
    unsafeInterleaveIO :: m a -> m a


    return = pure
    f >> g = f >>= (\x -> g)
    fail s = error s
    mfix = error "no mfix for this class"
    unsafeInterleaveIO = error "no unsafeInterleaveIO for this class"


instance Monad [] where
    xs >>= f = concatMap f xs

mapM f = sequence . map f

mapM_ f = sequence_ . map f

forM = flip mapM

forM_ = flip mapM_

sequence []     = return []
sequence (a:as) = do x <- a
                     xs <- sequence as
                     return (x:xs)

sequence_ [] = return ()
sequence_ (a:as) = do a
                      sequence_ as
                      return ()

(=<<) = flip (>>=)

infixr 1 <=<, >=>

f >=> g = \x -> do y <- f x
                   g y

f <=< g = flip (>=>)

forever as = do as
                forever as

--

join x = x >>= id

replicateM n a | n <= 0    = return []
               | otherwise = do x <- a
                                xs <- replicateM (n-1) a
                                return (x:xs)

replicateM_ n a | n <= 0     = return ()
                | otherwise  = do a
                                  replicateM_ (n-1) a

--

--guard

--when

--unless

---


liftM f a = do x <- a
               return $ f x

liftM2 f a b = do x <- a
                  y <- b
                  return $ f x y

liftM3 f a b c = do x <- a
                    y <- b
                    z <- c
                    return $ f x y z

liftM4 f a b c d = do x <- a
                      y <- b
                      z <- c
                      w <- d
                      return $ f x y z w

liftM5 f a b c d e = do x <- a
                        y <- b
                        z <- c
                        w <- d
                        u <- e
                        return $ f x y z w u

ap mf mx = do f <- mf
              x <- mx
              return $ f x
