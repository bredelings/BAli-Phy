{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Function (($))
import Data.Maybe
import System.IO (putStrLn)
import Text.Show (show)

main = do
  putStrLn $ show $ (case (Nothing :: Maybe Int) of
    Just x -> x + 1)
