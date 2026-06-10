{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Num
import Data.Function (($))
import System.IO (putStrLn)
import Text.Show (show)

bar x y = x + (y + 1)

main = do putStrLn $ show $ bar (2 :: Int) 3
