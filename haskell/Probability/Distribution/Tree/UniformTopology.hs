module Probability.Distribution.Tree.UniformTopology where

import           Tree
import           Probability.Random
import           Probability.Distribution.Uniform
import           Probability.Distribution.Independent
import           Probability.Distribution.List
import           Probability.Distribution.Exponential
import           Probability.Distribution.Categorical
import           MCMC
import           Data.Array
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import           Data.Text (Text)

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
sampleUniformTopology 0 = return $ Tree $ Forest $ Graph (IntMap.empty) (IntMap.empty) (IntMap.empty) (IntMap.empty) (IntMap.empty) (Attributes [])
sampleUniformTopology 1 = return $ Tree $ Forest $ Graph (IntMap.singleton 0 (Node 0 IntSet.empty)) (IntMap.empty) (IntMap.empty) (IntMap.singleton 0 noAttributes) (IntMap.empty) (Attributes [])
sampleUniformTopology n = do
    let num_nodes = 2 * n - 2
    edges <- uniformTopologyEdges [0 .. n - 1] [n .. num_nodes - 1]
    return $ treeFromEdges [0..num_nodes-1] edges

{- NOTE: No shuffling of taxa in uniformLabelledTopology

I think we don't need to shuffle the taxa here because the
locations of the node ids are already exchangeable.

Also, shuffling the taxa is probably something we don't want
to be MCMC resampling.
-}

uniformLabelledTopology taxa = do
  topology <- sample $ uniformTopology (length taxa)
  return $ addLabels (zip [0..] taxa) topology



uniformTopologyEffect tree = do
  -- SPR moves aren't added here because they depend on branch lengths.
  -- Note that we could in theory have multiple branch-length-trees with the same topology.
  add_move $ walkTreeSampleNNI tree -- Q: does this handle situations with no data partitions?

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
    type Result UniformTopology = Tree ()
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

uniformLabelledTree taxa branchLengthsDist = do
  topology <- RanSamplingRate 0 $ uniformLabelledTopology taxa
  branchLengths <- RanSamplingRate 0 $ sample $ iidMap (getUEdgesSet topology) branchLengthsDist
  let tree = branchLengthTree topology branchLengths
  addTreeMoves 1 tree
  return tree

uniformLabelledTree' :: [l] -> (forall t. IsTree t => t -> Random (IntMap Double)) -> Random (WithBranchLengths (Tree l))
uniformLabelledTree' taxa dist = do
  topology <- RanSamplingRate 0 $ uniformLabelledTopology taxa
  branchLengths <- RanSamplingRate 0 $ sample $ (dist topology)
  let tree = branchLengthTree topology branchLengths
  addTreeMoves 1 tree
  return tree

-- If we put the branch lengths under SamplingRate 0.0 then the maybe-polytomy trees won't work.
-- How can we do walk_tree and then run the MCMC kernels that affect a given branch?
-- The branch dist here depends on both the topology and the branch index.
uniformLabelledTree'' taxa branchLengthsDist = do
  topology <- RanSamplingRate 0.0 $ uniformLabelledTopology taxa
  branchLengths <- {- No RanSamplingRate 0-} sample $ independent $ (getUEdgesSet topology & IntMap.fromSet (branchLengthsDist topology))
  let tree = branchLengthTree topology branchLengths
  addTreeMoves 1 tree
  return tree

fixedTopologyTree topology dist = do
  branchLengths <- RanSamplingRate 0 $ sample $ iidMap (getUEdgesSet topology) dist
  let tree = branchLengthTree topology branchLengths
  addLengthMoves 1 tree
  return tree

-- | Initialize tree from loaded value and enable full tree moves
-- The tree structure comes from parsing a Newick file, but we make it
-- modifiable by adding MCMC moves. This differs from fixedTopologyTree
-- because both topology and branch lengths can change.
--
-- The input tree from Newick parsing includes WithRoots wrapper, which we strip
-- since BAli-Phy uses unrooted trees internally.
initialTreeWithMoves :: WithBranchLengths (WithRoots (Tree l)) -> Random (WithBranchLengths (Tree l))
initialTreeWithMoves (WithBranchLengths (WithRoots tree _ _) lengths) = do
  let unrootedTree = WithBranchLengths tree lengths
  addTreeMoves 1 unrootedTree
  return unrootedTree

uniformRootedTopology n = do
  topology <- sample $ uniformTopology n
  root <- sample $ uniformCategoricalOn (nodes topology)
  return $ addRoot root topology

uniformRootedTree taxa branchLengthsDist = do
  tree <- uniformLabelledTree taxa branchLengthsDist
  root <- sample $ uniformCategoricalOn (nodes tree)
  return $ addRoot root tree
