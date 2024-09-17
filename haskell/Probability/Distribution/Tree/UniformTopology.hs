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
uniform_topology_edges [l1]     _        = return []
uniform_topology_edges [l1, l2] _        = return [(l1, l2)]
uniform_topology_edges (l : ls) (i : is) = do
    es1           <- uniform_topology_edges ls is
    ((x, y), es2) <- removeOne es1
    return $ [(l, i), (x, i), (i, y)] ++ es2

-- We could rewrite uniform_topology_edges to automatically flip and sort the branches with leaf branches first.
sample_uniform_topology 1 = return $ Tree $ Forest $ Graph (IntMap.singleton 0 (Node 0 IntSet.empty)) (IntMap.empty) (IntMap.singleton 0 noAttributes) (IntMap.singleton 0 noAttributes) (Attributes [])
sample_uniform_topology n = do
    let num_nodes = 2 * n - 2
    edges <- uniform_topology_edges [0 .. n - 1] [n .. num_nodes - 1]
    return $ tree_from_edges [0..num_nodes-1] edges

uniform_labelled_topology taxa = do
  topology <- sample $ uniformTopology (length taxa)
  return $ add_labels (zip [0..] taxa) topology



uniform_topology_effect tree = do
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
uniform_topology_pr 1 = 1
uniform_topology_pr 2 = 1
uniform_topology_pr n = uniform_topology_pr (n - 1) / (fromIntegral $ 2 * n - 5)

-------------------------------------------------------------
data UniformTopology = UniformTopology Int

instance Dist UniformTopology where
    type Result UniformTopology = Tree
    dist_name _ = "uniform_topology"

instance HasAnnotatedPdf UniformTopology where
    annotated_densities (UniformTopology n) _ = return ([uniform_topology_pr n], ())

instance Sampleable UniformTopology where
    sample dist@(UniformTopology n) = RanDistribution3 dist uniform_topology_effect triggeredModifiableTree (sample_uniform_topology n)

uniformTopology n = UniformTopology n


-------------------
{-
tree ~ uniformLabelledTree(taxa, function(topology: gamma(0.5, 2/numBranches(topology))))

tree ~ fixedTopologyTree(readTopology(filename), function(topology: gamma(0.5, 2/numBranches(topology) ) ) )
-}

uniformLabelledTree taxa dist = do
  topology <- RanSamplingRate 0 $ sample $ uniform_labelled_topology taxa
  branchLengths <- RanSamplingRate 0 $ sample $ iidMap (getUEdgesSet topology) (dist topology)
  let tree = branch_length_tree topology branchLengths
  addTopologyMoves 2 tree
  addLengthMoves 2 tree
  return tree

fixedTopologyTree topology dist = do
  branchLengths <- RanSamplingRate 0 $ sample $ iidMap (getUEdgesSet topology) (dist topology)
  let tree = branch_length_tree topology branchLengths
  addMove 1 $ walk_tree_sample_branch_lengths tree
  return tree

uniform_labelled_tree taxa branch_lengths_dist = do
  -- These lines should be under SamplingRate 0.0 -- but then the polytomy trees won't work
  topology <- RanSamplingRate 0.0 $ uniform_labelled_topology taxa
  -- Q. How can we do walk_tree and then run the MCMC kernels that affect a given branch?
--  branch_lengths <- sample $ independent [branch_lengths_dist topology b | b <- [0..numBranches topology-1]]
  branch_lengths <- sample $ independent $ (getUEdgesSet topology & IntMap.fromSet (branch_lengths_dist topology))
  let tree = WithBranchLengths topology branch_lengths
  return tree `with_tk_effect` addTreeMoves 1

