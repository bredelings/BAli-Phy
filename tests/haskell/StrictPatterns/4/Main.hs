{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Function (($))
import System.IO (putStrLn)
import Text.Show (show)

main = do
  let !(x,y) = (1 :: Int, 2 :: Int)
  putStrLn $ show $ x + y
