{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Error (error)
import Data.Maybe
import System.IO (putStrLn)

undefined = error "undefined"

main = do
  let !(Just x) = Just undefined
  putStrLn "matched"
