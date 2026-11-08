{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Exception
import System.IO (IO, print, putStrLn)

bad :: Int
bad = throw (IOException "boom")

handle message _ = putStrLn message

-- Check that ordinary IO can catch the same exception twice; distinct markers verify both catches.
main = do
  catch (print bad) (handle "first caught")
  catch (print bad) (handle "second caught")
