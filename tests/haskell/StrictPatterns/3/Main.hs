{-# LANGUAGE NoImplicitPrelude #-}

import Data.Maybe
import System.IO (putStrLn)

main = do
  let !(Just x) = Nothing
  putStrLn "unreachable"
