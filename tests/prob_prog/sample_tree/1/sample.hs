module Model where

import           Probability

import           Tree
import           Tree.Newick

model = do
    tree <- sample $ uniformTopology 5
    return ["tree" %=% writeNewick tree]

main = do
  return model
