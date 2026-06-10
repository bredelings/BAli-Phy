{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Function (($))
import Data.Maybe
import System.IO (putStrLn)
import Text.Show (show)

f :: Maybe Int -> Int
f (Just x) = x

main = do
  putStrLn $ show $ f Nothing + (1 :: Int)
