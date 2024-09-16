module Probability.Distribution.Tree (module Probability.Distribution.Tree,
                                      module Probability.Distribution.Tree.UniformTopology,
                                      module Probability.Distribution.Tree.Moves)
    where

import           Tree
import           Probability.Random
import           Probability.Distribution.Uniform
import           Probability.Distribution.List
import           Probability.Distribution.Exponential
import           MCMC
import           Data.Array
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import           Probability.Distribution.Tree.Moves
import           Probability.Distribution.Tree.Modifiable
import           Probability.Distribution.Tree.UniformTopology

remove_n 0 list = return ([], list)
remove_n n list = do
    (x , list_minus_1) <- remove_one list
    (xs, list_minus_n) <- remove_n (n - 1) list_minus_1
    return ((x : xs), list_minus_n)

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
    ([l1, l2], leaves') <- remove_n 2 leaves
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

-- leaves   nodes  branches
-- 1        1      0
-- 2        3      2
-- 3        5      4
-- 4        7      6
modifiable_rooted_tree :: (forall a.a -> a) -> WithRoots Tree -> WithRoots Tree
modifiable_rooted_tree modf (WithRoots tree [root_node] _) = add_root root_node $ modifiable_tree modf tree
-- Is it still true that we need the root node to have a constrant degree?

triggered_modifiable_rooted_tree = triggered_modifiable_structure modifiable_rooted_tree

-- A uniform-ordered-history distribution would need to augment nodes with an Int order, instead of a Double order.

-- maybe modf has type (forall a . a -> a)?
-- we should be able to apply it to both Int and Double...
modifiable_time_tree :: (forall a.a -> a) -> WithNodeTimes (WithRoots Tree) -> WithNodeTimes (WithRoots Tree)
modifiable_time_tree modf (WithNodeTimes rooted_tree' times') = WithNodeTimes rooted_tree times where
    rooted_tree = modifiable_rooted_tree modf rooted_tree'
    maybe_modf :: Int -> a -> a
    maybe_modf node x | node < numLeaves rooted_tree'   = x
                      | otherwise                       = modf x
    times     = IntSet.fromList [0..numNodes rooted_tree'-1] & IntMap.fromSet (\node -> maybe_modf node (times' IntMap.! node))

triggered_modifiable_time_tree = triggered_modifiable_structure modifiable_time_tree

-- Add moves for non-root internal-node times.
-- FIXME: check that the leaves times are fixed?
-- FIXME: check that numLeaves tree is not changeable?
uniform_time_tree_effect tree = sequence_ [ add_move $ slice_sample_real_random_variable (node_time tree node) (above 0.0)
                                          | node <- [numLeaves tree..numNodes tree - 1], node /= root tree
                                          ]

-- Add moves for internal-node times INCLUDING the root.
-- FIXME: check that the leaves times are fixed?
-- FIXME: check that numLeaves tree is not changeable?
coalescent_tree_effect tree = do
  sequence_ [ add_move $ slice_sample_real_random_variable (node_time tree node) (above 0.0)
            | node <- internal_nodes tree]

  sequence_ [ add_move $ metropolis_hastings $ fnpr_unsafe_proposal tree node
            | node <- getNodes tree]

  sequence_ [ add_move $ tnni_on_branch_unsafe tree branch
            | branch <- getEdges tree]



data CoalEvent = Leaf | Internal | RateShift Double
node_type tree node = if is_leaf_node tree node then Leaf else Internal

coalescent_tree_pr_factors theta n_leaves tree = go 0 0 (2/theta) 1 times: parent_before_child_prs n_leaves tree
    where times = sortOn fst [ (node_time tree node, node_type tree node) | node <- [0..numNodes tree - 1]]
          go prev_time n rate pr [] = pr
          go prev_time n rate pr ((time,event):events) =
              let delta_t = time - prev_time
                  n_choose_2 = fromIntegral $ (n*(n-1)) `div` 2
                  rate_all = rate * n_choose_2
                  pr_nothing = doubleToLogDouble $ exp $ (-rate_all * delta_t)
                  pr' = pr * pr_nothing
              in case event of Leaf     -> go time (n+1) rate pr' events
                               -- For Internal, we divided out the n_choose2
                               Internal -> go time (n-1) rate (pr' * (doubleToLogDouble rate)) events
                               RateShift new_rate -> go time n new_rate pr' events


-- This doesn't handle serially-sampled tips... for that we would need to
-- * modify sample_uniform_ordered_tree
-- * pass in a list of (node,time) pairs.

-- We would sort and merge the Leaf and RateShift r events, and then
-- add the Internal events (effectively -- we would also need to
-- Or should it be (name,time) pairs?

sample_coalescent_tree theta n_leaves = do
  topology <- sample_uniform_ordered_tree n_leaves

  let rate = 2/theta
  dts <- sequence [ sample $ exponential (1 / (rate* n_choose_2) )| n <- reverse [2..n_leaves],
                                                                    let n_choose_2 = fromIntegral $ n*(n-1) `div` 2]
  let times = (replicate n_leaves 0) ++ (scanl1 (+) dts)
      node_times = IntMap.fromList $ zip [0..] times
  return (time_tree topology node_times)


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

-------------------------------------------------------------
data CoalescentTree = CoalescentTree Double Int

instance Dist CoalescentTree where
    type Result CoalescentTree = WithNodeTimes (WithRoots Tree)
    dist_name _ = "uniform_time_tree"

instance HasAnnotatedPdf CoalescentTree where
    annotated_densities (CoalescentTree theta n) tree = return (coalescent_tree_pr_factors theta n tree, ())

instance Sampleable CoalescentTree where
    sample dist@(CoalescentTree theta n) = RanDistribution3 dist coalescent_tree_effect triggered_modifiable_time_tree (sample_coalescent_tree theta n)

coalescent_tree theta n = CoalescentTree theta n
