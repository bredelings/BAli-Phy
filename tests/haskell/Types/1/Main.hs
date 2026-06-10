{-# LANGUAGE NoImplicitPrelude, RankNTypes, ScopedTypeVariables #-}

module Main where

import Compiler.Num
import Data.Function (($), id)
import System.IO (putStrLn)
import Text.Show (show)

polyapply :: (forall a.a->a) -> (Int, Char)
polyapply f = (f 1, f 'a')

polyapply2 :: (forall a.a->a) -> (Int, Char)
polyapply2 (f :: forall a.a->a) = (f 1, f 'a')

main = do putStrLn "HelloWorld!"
          putStrLn $ show [[1 :: Int],[2]]
          putStrLn $ show (1 :: Int,'a')
          putStrLn $ show 'a'
          putStrLn $ show $ polyapply (id :: forall b.b->b)
          putStrLn $ show $ polyapply id
          putStrLn $ show $ polyapply2 (id :: forall b.b->b)
          putStrLn $ show $ polyapply2 id
