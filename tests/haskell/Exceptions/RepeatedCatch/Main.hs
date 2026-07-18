{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Exception
import System.IO (IO, print, putStrLn)

bad :: Int
bad = throw (IOException "boom")

handle :: IOException -> IO ()
handle _ = putStrLn "caught"

main = do
  catch (print bad) handle
  catch (print bad) handle
