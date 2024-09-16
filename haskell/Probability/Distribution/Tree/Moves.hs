module Probability.Distribution.Tree.Moves where

import MCMC
import Probability.Random    

addSPRMoves rate tree = do
  addMove rate $ sample_SPR_all tree
  addMove (rate/2) $ sample_SPR_flat tree
  addMove (rate/2) $ sample_SPR_nodes tree

addTopologyMoves rate tree = do
  addSPRMoves rate tree
  addMove rate $ walk_tree_sample_NNI_and_branch_lengths tree
  addMove (2*rate) $ walk_tree_sample_NNI tree  -- if alignment is fixed this is really cheap -- increase weight?
  addMove (0.5*rate) $ walk_tree_sample_NNI_and_A tree

addLengthMoves rate tree = do
  addMove rate $ walk_tree_sample_branch_lengths tree

addTreeMoves rate tree = do
  addTopologyMoves (rate*2) tree
  addLengthMoves (rate*2) tree

