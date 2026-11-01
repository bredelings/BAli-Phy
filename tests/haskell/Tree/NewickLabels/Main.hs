{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Data.Text.IO as Text
import System.IO (IO, putStrLn, print)
import Compiler.Num
import Data.Function (($))
import qualified Data.Text as T
import qualified Data.IntMap as IntMap
import Tree
import Tree.Newick (newickToTree, parse_newick, print_newick, writeNewick)

-- Parse and rewrite labels to check literal underscores and quoted spaces.
main :: IO ()
main = do
  (tree, _) <- newickToTree (parse_newick "(A_B,'C D');")
  Text.putStrLn (writeNewick tree)
  -- Nested brackets in an attribute must belong to the outer comment rather than ending it.
  putStrLn (print_newick (parse_newick "(A:[&note=[inner[deep]]]0.1,B);"))
  -- Attribute commas inside grouping characters must not split the surrounding field.
  putStrLn (print_newick (parse_newick "(A:[&a=(x,y),foreground={1,2},note=[x,y],z=3]0.1,B);"))

  -- Writer-only tests miss Show instances that discard outer wrappers. Keep these checks
  -- while Show delegates to Newick for the different tree representations.
  let topology = addLabels [(0,T.pack "A B"),(1,T.pack "C")] $
                 treeFromEdges [0,1,2] [(0,2),(1,2)]
      rooted = addRoot 2 topology
      lengths = WithBranchLengths topology (IntMap.fromSet (\_ -> 0.5) (getEdgesSet topology))
      times = time_tree rooted (IntMap.fromList [(0,0),(1,0),(2,1)])
      rates = IntMap.fromSet (\_ -> 2) (getUEdgesSet topology)
  print topology
  print rooted
  print lengths
  print times
  print (rate_time_tree times rates)
