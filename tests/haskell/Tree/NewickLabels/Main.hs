{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Data.Text.IO as Text
import System.IO (IO, putStrLn)
import Tree.Newick (newickToTree, parse_newick, print_newick, writeNewick)

-- Parse and rewrite labels to check literal underscores and quoted spaces.
main :: IO ()
main = do
  (tree, _) <- newickToTree (parse_newick "(A_B,'C D');")
  Text.putStrLn (writeNewick tree)
  -- Nested brackets in an attribute must belong to the outer comment rather than ending it.
  putStrLn (print_newick (parse_newick "(A:[&note=[inner[deep]]]0.1,B);"))
