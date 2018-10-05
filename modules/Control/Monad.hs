{-# LANGUAGE NoImplicitPrelude #-}
module Control.Monad where

import Compiler.Base
import Data.List
import Data.Function

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

f >=> g = \x -> do y <- f x
                   g y

f <=< g = flip (>=>)

void f = do x <- f
            return ()
--

liftM f a = do x <- a
               return $ f x

liftM2 f a b = do x <- a
                  y <- b
                  return $ f x y

liftM3 f a b c = do x <- a
                    y <- b
                    z <- c
                    return $ f x y c

-- LiftM4

-- LiftM5

ap mf mx = do f <- mf
              x <- mx
              return $ f x
