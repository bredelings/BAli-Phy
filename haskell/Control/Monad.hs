{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad where

import Compiler.Base
import Data.List
import Data.Function
import Data.Ord
import Compiler.Num

mapM f = sequence . map f

mapM_ f = sequence_ . map f

forM = flip mapM

forM_ = flip mapM_

sequence [] = return []
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

void f = do x <- f
            return ()
--

-- join is implemented in Compiler.base

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
