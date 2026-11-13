module Probability.Distribution.Tree.Moves where

import Tree (numNodes)
import MCMC
import Probability.Random    

addSPRMoves rate tree = do
  addMove rate $ sampleSPRAll tree
  addMove (rate/2) $ sampleSPRFlat tree
  addMove (rate/2) $ sampleSPRNodes tree

addTopologyMoves rate tree = do
  addSPRMoves rate tree
  addMove rate $ walkTreeSampleNNIandBranchLengths tree
  addMove (2*rate) $ walkTreeSampleNNI tree  -- if alignment is fixed this is really cheap -- increase weight?
  addMove (0.5*rate) $ walkTreeSampleNNIandA tree

addLengthMoves rate tree = do
  addMove rate $ walkTreeSampleBranchLengths tree

addTreeMoves rate tree = do
  addTopologyMoves (rate*2) tree
  addLengthMoves (rate*2) tree

-- Traverse the tree for NNI and node-age updates, and search SPR attachment locations.
-- A single-tip tree has no moves; node-age updates skip non-modifiable ages.
-- Use the fixed node set: counting leaves depends on branch connections that SPR temporarily
-- changes, which would invalidate move registration and force the prior on a pruned tree.
addTimeTreeMoves rate tree
    | numNodes tree == 1 = return ()
    | otherwise = do
        addMove (rate*2) $ walkTimeTreeSampleNNIandNodeTimes tree
        addMove (rate*2) $ sampleSPRAll tree
        return ()
