{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail (MonadFail)
import Compiler.Num
import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Data.Traversable
import System.IO
import Text.Read
import Text.Show

-- Protect the small compatibility surface used by later transformer and package ports.
-- This becomes obsolete when these modules are supplied by an upstream base package.
main = do
    let mapped = fmap ((+) 1) (Identity (2 :: Int))
        applied = pure ((*) 2) <*> Identity (4 :: Int)
        bound = Identity (5 :: Int) >>= (Identity . ((+) 1))
        parsed = read (show mapped) :: Identity Int
        folded = toList (Identity (7 :: Int))
        traversed = traverse (Just . ((+) 1)) (Identity (8 :: Int))
        pair = bimap ((+) 1) ((*) 2) (3 :: Int, 4 :: Int)
        leftValue = first ((+) 2) (Left (5 :: Int) :: Either Int Int)
        rightValue = second ((+) 3) (Right (6 :: Int) :: Either Int Int)
    putStrLn $ show ((mapped, applied, bound, parsed), (folded, traversed, pair, leftValue), rightValue)

usesMonadFail :: MonadFail m => m a -> m a
usesMonadFail x = x
