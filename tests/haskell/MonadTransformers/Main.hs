{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Base
import Compiler.Num
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Either
import Data.Function
import Data.Functor.Identity
import Data.List
import System.IO
import Text.Show

readerExample :: Reader Int (Int, Int)
readerExample = do
    outer <- ask
    inner <- local ((+) 1) ask
    return (outer, inner)

exceptExample :: Except String String
exceptExample = catchE (throwE "bad") (return . (++ "!"))

alternativeExample :: Except String ()
alternativeExample = throwE "a" <|> throwE "b"

stackExample :: ReaderT Int (ExceptT String Identity) Int
stackExample = do
    value <- ask
    lift (return (value + 1))

stateExample :: State Int (Int, Int)
stateExample = do
    before <- get
    modify ((+) 1)
    doubled <- gets ((*) 2)
    return (before, doubled)

-- Protect transformer stacking and higher-kinded MonadTrans inference, which ordinary module
-- typechecking does not exercise. This can be removed when an upstream transformers suite runs.
main = putStrLn $ show
    ( runReader readerExample 4
    , runExcept exceptExample
    , runExcept alternativeExample
    , ( runIdentity (runExceptT (runReaderT stackExample 4))
      , runState stateExample 4
      )
    )
