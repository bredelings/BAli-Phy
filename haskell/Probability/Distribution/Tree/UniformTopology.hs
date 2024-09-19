module Probability.Distribution.Tree.UniformTopology where

import           Tree
import           Probability.Random
import           Probability.Distribution.Uniform
import           Probability.Distribution.List
import           Probability.Distribution.Exponential
import           MCMC
import           Data.Array
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import           Probability.Distribution.Tree.Util
import           Probability.Distribution.Tree.Modifiable    
import           Probability.Distribution.Tree.Moves

-- Create a tree of size n-1, choose an edge at random, and insert the next leaf there.
uniformTopologyEdges [l1]     _        = return []
uniformTopologyEdges [l1, l2] _        = return [(l1, l2)]
uniformTopologyEdges (l : ls) (i : is) = do
    es1           <- uniformTopologyEdges ls is
    ((x, y), es2) <- removeOne es1
    return $ [(l, i), (x, i), (i, y)] ++ es2

-- We could rewrite uniform_topology_edges to automatically flip and sort the branches with leaf branches first.
sampleUniformTopology 1 = return $ Tree $ Forest $ Graph (IntMap.singleton 0 (Node 0 IntSet.empty)) (IntMap.empty) (IntMap.singleton 0 noAttributes) (IntMap.singleton 0 noAttributes) (Attributes [])
sampleUniformTopology n = do
    let num_nodes = 2 * n - 2
    edges <- uniformTopologyEdges [0 .. n - 1] [n .. num_nodes - 1]
    return $ treeFromEdges [0..num_nodes-1] edges

uniformLabelledTopology taxa = do
  topology <- sample $ uniformTopology (length taxa)
  return $ addLabels (zip [0..] taxa) topology



uniformTopologyEffect tree = do
  -- SPR moves aren't added here because they depend on branch lengths.
  -- Note that we could in theory have multiple branch-length-trees with the same topology.
  add_move $ walk_tree_sample_NNI tree -- Q: does this handle situations with no data partitions?

-------------------------------------------------------------

{-
   leaves   nodes  branches
   1        1      0
   2        2      1
   3        4      3
   4        6      5
   5        8      7
   ..       ..     ..
-}
uniformTopologyPr 1 = 1
uniformTopologyPr 2 = 1
uniformTopologyPr n = uniformTopologyPr (n - 1) / (fromIntegral $ 2 * n - 5)

-------------------------------------------------------------
data UniformTopology = UniformTopology Int

instance Dist UniformTopology where
    type Result UniformTopology = Tree
    dist_name _ = "uniform_topology"

instance HasAnnotatedPdf UniformTopology where
    annotated_densities (UniformTopology n) _ = return ([uniformTopologyPr n], ())

instance Sampleable UniformTopology where
    sample dist@(UniformTopology n) = RanDistribution3 dist uniformTopologyEffect triggeredModifiableTree (sampleUniformTopology n)

uniformTopology n = UniformTopology n


-------------------
{-
tree ~ uniformLabelledTree(taxa, function(topology: gamma(0.5, 2/numBranches(topology))))

tree ~ fixedTopologyTree(readTopology(filename), function(topology: gamma(0.5, 2/numBranches(topology) ) ) )
-}

uniformLabelledTree taxa dist = do
  topology <- RanSamplingRate 0 $ uniformLabelledTopology taxa
  branchLengths <- RanSamplingRate 0 $ sample $ iidMap (getUEdgesSet topology) (dist topology)
  let tree = branchLengthTree topology branchLengths
  addTreeMoves 1 tree
  return tree

-- If we put the branch lengths under SamplingRate 0.0 then the maybe-polytomy trees won't work.
-- How can we do walk_tree and then run the MCMC kernels that affect a given branch?
uniform_labelled_tree taxa branchLengthsDist = do
  topology <- RanSamplingRate 0.0 $ uniformLabelledTopology taxa
  branchLengths <- sample $ independent $ (getUEdgesSet topology & IntMap.fromSet (branchLengthsDist topology))
  let tree = branchLengthTree topology branchLengths
  addTreeMoves 1 tree
  return tree

fixedTopologyTree topology dist = do
  branchLengths <- RanSamplingRate 0 $ sample $ iidMap (getUEdgesSet topology) (dist topology)
  let tree = branchLengthTree topology branchLengths
  addMove 1 $ walk_tree_sample_branch_lengths tree
  return tree

