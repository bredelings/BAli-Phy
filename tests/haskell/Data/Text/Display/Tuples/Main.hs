{-# LANGUAGE NoImplicitPrelude #-}
import Compiler.Num
import Data.Text (unpack)
import Data.Text.Display (display)
import System.IO (putStrLn)

-- Display has independent tuple formatting, so Show tests cannot detect mismatched
-- delimiters here. This check is unnecessary if the implementations are unified.
main = do
    putStrLn (unpack (display (1 :: Int,2 :: Int)))
    putStrLn (unpack (display (1 :: Int,2 :: Int,3 :: Int)))
    putStrLn (unpack (display (1 :: Int,2 :: Int,3 :: Int,4 :: Int)))
