module Model where

import           Probability

import           Tree
import           Tree.Newick

model = do
    tree <- sample $ uniformTopology 5
    return ["tree" %=% write_newick tree]

main = do
  return model
