{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Data.Text.IO as Text
import System.IO (IO)
import Tree.Newick (newickToTree, parse_newick, writeNewick)

-- Parse and rewrite labels to check literal underscores and quoted spaces.
main :: IO ()
main = do
  (tree, _) <- newickToTree (parse_newick "(A_B,'C D');")
  Text.putStrLn (writeNewick tree)
