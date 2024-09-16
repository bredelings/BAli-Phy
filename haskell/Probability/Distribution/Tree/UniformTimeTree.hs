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

uniform_ordered_tree_edges [l1]     _        = return []
uniform_ordered_tree_edges leaves   (i : is) = do
    ([l1, l2], leaves') <- remove 2 leaves
    other_edges         <- uniform_ordered_tree_edges (i : leaves') is
    return $ [(l1, i), (l2, i)] ++ other_edges

sample_uniform_ordered_tree n = do
  let num_nodes = 2 * n - 1
  edges <- uniform_ordered_tree_edges [0..n-1] [n..]
  -- The number of edges should be 2*n-1, unchangably.
  let utree = tree_from_edges [0..num_nodes-1] edges
  return $ add_root (num_nodes - 1) utree

sample_uniform_time_tree age n = do
  topology <- sample_uniform_ordered_tree n
  times <- sort <$> (sample $ iid (n-2) (uniform 0.0 age))
  let all_times = replicate n 0.0 ++ times ++ [age]
      all_node_times = IntMap.fromList $ zip [0..] all_times
  return $ time_tree topology all_node_times

possible = 1 :: LogDouble
impossible = 0 :: LogDouble
require p = if p then possible else impossible

parent_before_child_prs n_leaves tree = [factor n | n <- [0 .. 2*n_leaves-2] ]
    where time = node_time tree
          factor n = case parentNode tree n of Nothing -> possible
                                               Just p  -> require $ time n <= time p

uniform_time_tree_pr age n_leaves tree = factor0 : parent_before_child_prs n_leaves tree
    where factor0 = doubleToLogDouble age `pow` fromIntegral (2-n_leaves)

-- Add moves for non-root internal-node times.
-- FIXME: check that the leaves times are fixed?
-- FIXME: check that numLeaves tree is not changeable?
uniform_time_tree_effect tree = sequence_ [ add_move $ slice_sample_real_random_variable (node_time tree node) (above 0.0)
                                          | node <- [numLeaves tree..numNodes tree - 1], node /= root tree
                                          ]

-- This doesn't handle serially-sampled tips... for that we would need to
-- * modify sample_uniform_ordered_tree
-- * pass in a list of (node,time) pairs.


-------------------------------------------------------------
data UniformTimeTree = UniformTimeTree Double Int

instance Dist UniformTimeTree where
    type Result UniformTimeTree = WithNodeTimes (WithRoots Tree)
    dist_name _ = "uniform_time_tree"

instance HasAnnotatedPdf UniformTimeTree where
    annotated_densities (UniformTimeTree age n) tree = return (uniform_time_tree_pr age n tree, ())

instance Sampleable UniformTimeTree where
    sample dist@(UniformTimeTree age n) = RanDistribution3 dist uniform_time_tree_effect triggered_modifiable_time_tree (sample_uniform_time_tree age n)

uniform_time_tree age n = UniformTimeTree age n

