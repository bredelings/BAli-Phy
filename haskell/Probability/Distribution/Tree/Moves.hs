module Probability.Distribution.Tree.Moves where

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

addTimeTreeMoves rate tree = do
  -- Resample all the node times, including the root...
  -- But what if some node times are fixed?
  -- FIXME: check that leaf times are fixed?
  -- sequence_ [ addMove 1 $ sliceSample (nodeTime tree node) (above 0) | node <- internalNodes tree]

  -- This allow attaching at the same level OR more rootward.
  -- FIXME: but it doesn't allow becoming the root!
  -- QUESTION: Could we slice sample the root location?
  -- QUESTION: Could we somehow propose a root location based on the likelihood/posterior?
  -- sequence_ [ addMove 1 $ metropolisHastings $ fnpr_unsafe_proposal tree node | node <- getNodes tree]

  -- Exchange sibling branches with children?
  addMove (rate*2) $ walkTimeTreeSampleNNIandNodeTimes tree

  addMove (rate*2) $ sampleSPRAll tree
