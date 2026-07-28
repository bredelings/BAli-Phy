{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Data.Text.IO as Text
import System.IO (IO)
import Tree.Newick (readTreeTopology, writeNewick)

main :: IO ()
main = do
  tree <- readTreeTopology "bad.tree"
  Text.putStrLn (writeNewick tree)
