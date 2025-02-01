module Probability.Distribution.Tree.UniformTimeTree where

import Tree
import Probability.Random
import Probability.Distribution.List
import Probability.Distribution.Tree.Modifiable
import Probability.Distribution.Tree.Util
import Probability.Distribution.Uniform
import qualified Data.IntMap as IntMap
import MCMC

----
-- choose 2 leaves, connect them to an internal node, and put that internal node on the list of leaves
-- This is I think gives more weight to more balanced trees?

-- * actually I'm not sure the likelihood handles degree-2 nodes.
-- * imodels might not handle degree-2 nodes.
-- * we also assume that each node has a constant degree.
--   + can we ensure that the root index is constant, and also the highest?
-- * can we map the rooted tree onto an unrooted tree with the root removed?  not sure..
--   + this makes reconstructing the ancestral sequence at the root more challenging.

uniformOrderedTreeEdges [l1]     _        = return []
uniformOrderedTreeEdges leaves   (i : is) = do
    tmp <- remove 2 leaves
    let Just ([l1, l2], leaves') = tmp
    otherEdges         <- uniformOrderedTreeEdges (i : leaves') is
    return $ [(l1, i), (l2, i)] ++ otherEdges

sampleUniformOrderedTree n = do
  let numNodes = 2 * n - 1
  edges <- uniformOrderedTreeEdges [0..n-1] [n..]
  -- The number of edges should be 2*n-1, unchangably.
  let utree = treeFromEdges [0..numNodes-1] edges
  return $ addRoot (numNodes - 1) utree

sampleUniformTimeTree age n = do
  topology <- sampleUniformOrderedTree n
  times <- sort <$> (sample $ iid (n-2) (uniform 0 age))
  let allTimes = replicate n 0 ++ times ++ [age]
      allNodeTimes = IntMap.fromList $ zip [0..] allTimes
  return $ time_tree topology allNodeTimes

uniformTimeTreePr age nLeaves tree = factor0 : parentBeforeChildPrs tree
    where factor0 = doubleToLogDouble age `pow` fromIntegral (2-nLeaves)

-- Add moves for non-root internal-node times.
-- FIXME: check that the leaves times are fixed?
-- FIXME: check that numLeaves tree is not changeable?
uniformTimeTreeEffect tree = sequence_ [ addMove 1 $ sliceSample (nodeTime tree node) (above 0.0)
                                       | node <- [numLeaves tree..numNodes tree - 1], node /= root tree
                                       ]

-- This doesn't handle serially-sampled tips... for that we would need to
-- * modify sample_uniform_ordered_tree
-- * pass in a list of (node,time) pairs.


-------------------------------------------------------------
data UniformTimeTree = UniformTimeTree Double Int

instance Dist UniformTimeTree where
    type Result UniformTimeTree = WithNodeTimes (WithRoots (Tree ()))
    dist_name _ = "uniformTimeTree"

instance HasAnnotatedPdf UniformTimeTree where
    annotated_densities (UniformTimeTree age n) tree = return (uniformTimeTreePr age n tree, ())

instance Sampleable UniformTimeTree where
    sample dist@(UniformTimeTree age n) = RanDistribution3 dist uniformTimeTreeEffect triggeredModifiableTimeTree (sampleUniformTimeTree age n)

uniformTimeTree age n = UniformTimeTree age n

