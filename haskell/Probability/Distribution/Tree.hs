module Probability.Distribution.Tree where

import           Tree
import           Probability.Random
import           Probability.Distribution.Uniform
import           Probability.Distribution.List
import           MCMC

xrange start end | start < end = start : xrange (start + 1) end
                 | otherwise   = []

pick_index 0 (h : t) = (h, t)
pick_index 0 []      = error "Trying to pick from empty list!"
pick_index i (h : t) = let (x, t2) = pick_index (i - 1) t in (x, h : t2)

remove_one []   = error "Cannot remove one from empty list"
remove_one list = do
    i <- uniform_int 0 (length list - 1)
    return $ pick_index i list

remove_n 0 list = return ([], list)
remove_n n list = do
    (x , list_minus_1) <- remove_one list
    (xs, list_minus_n) <- remove_n (n - 1) list_minus_1
    return ((x : xs), list_minus_n)

-- Create a tree of size n-1, choose an edge at random, and insert the next leaf there.
uniform_topology_edges [l1]     _        = return []
uniform_topology_edges [l1, l2] _        = return [(l1, l2)]
uniform_topology_edges (l : ls) (i : is) = do
    es1           <- uniform_topology_edges ls is
    ((x, y), es2) <- remove_one es1
    return $ [(l, i), (x, i), (i, y)] ++ es2

-- We could rewrite uniform_topology_edges to automatically flip and sort the branches with leaf branches first.
sample_uniform_topology 1 = return $ Tree (listArray' [[]]) (listArray' []) 1
sample_uniform_topology n = do
    let num_nodes = 2 * n - 2
    edges <- uniform_topology_edges [0 .. n - 1] [n .. num_nodes - 1]
    -- This flipping is suppose flip edges from (internal,leaf) -> (leaf, internal)
    let maybe_flip (x, y) | (y < x)   = (y, x)
                          | otherwise = (x, y)
    -- Then the sorting is supposed order edges like (0,_), (1,_), (2,_)
    -- in order to assign leaf branches the names 0..n-1
    let sorted_edges     = sortOn fst $ map maybe_flip edges
    -- The number of edges should be 2*n-3, unchangably.
    return $ tree_from_edges num_nodes sorted_edges


force_tree tree@(Tree nodes branches n_nodes) = force_nodes `seq` force_branches where
    n_branches = numBranches tree
    force_nodes    = force_struct $ listArray' [ force_list $ edgesOutOfNode tree node | node <- xrange 0 n_nodes ]
    force_branches = force_struct $ listArray' [ force_struct $ nodesForEdge tree b | b <- xrange 0 (n_branches * 2)]

-- leaves   nodes  branches
-- 1        1      0
-- 2        2      1
-- 3        4      3
-- 4        6      5
modifiable_cayley_tree modf tree = Tree (listArray' nodes) (listArray' branches) n_nodes where
    n_nodes = numNodes tree
    n_leaves | n_nodes == 1  = 1
             | otherwise     = (n_nodes+2) `div` 2
    n_branches = n_nodes - 1
    degree node | n_leaves == 1   = 0
                | node < n_leaves = 1
                | otherwise       = 3
    nodes    = [ mapn (degree node) modf (edgesOutOfNode tree node) | node <- xrange 0 n_nodes ]
    branches = [ (modf s, modf i, modf t, (b + n_branches) `mod` (2*n_branches)) | b <- xrange 0 (n_branches * 2), let (s, i, t, _) = nodesForEdge tree b ]

-- our current modifiable tree structure requires the node to have a constrant degree.


uniform_topology_pr 1 = doubleToLogDouble 1.0
uniform_topology_pr 2 = doubleToLogDouble 1.0
uniform_topology_pr n = uniform_topology_pr (n - 1) / (doubleToLogDouble $ intToDouble $ 2 * n - 5)

-- The *triggered* tree is lazy: when we access anything that is modifiable, it triggers all effects,
-- which includes forcing all the modifiables in the *untriggered* tree.

-- We don't want to force all fields of the tree when _any_ tree field is accessed, only when a _random_ field is accessed.
-- This is why triggered tree still uses 'tree' as input to 'modifiable_tree'.
triggered_modifiable_tree value effect = (raw_tree, triggered_tree) where
    raw_tree       = modifiable_cayley_tree modifiable value
    effect'        = force_tree raw_tree `seq` effect
    triggered_tree = modifiable_cayley_tree (effect' `seq`) raw_tree

uniform_topology_effect tree = tree `seq` add_move (walk_tree_sample_nni_unsafe tree)

uniform_topology n = Distribution (\tree -> [uniform_topology_pr n])
                                  (no_quantile "uniform_topology")
                                  (RandomStructure uniform_topology_effect triggered_modifiable_tree (sample_uniform_topology n))
                                  (TreeRange n)

uniform_labelled_topology taxa = do
  topology <- uniform_topology (length taxa)
  return $ add_labels taxa topology

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
  let sorted_edges     = sortOn fst edges
  -- The number of edges should be 2*n-1, unchangably.
  let utree = tree_from_edges num_nodes sorted_edges
  return $ add_root (num_nodes - 1) utree

sample_uniform_time_tree age n = do
  topology <- sample_uniform_ordered_tree n
  times <- sort <$> iid (n-2) (uniform 0.0 age)
  let all_times = replicate n 0.0 ++ times ++ [age]
  return $ time_tree topology all_times

possible = doubleToLogDouble 1.0
impossible = doubleToLogDouble 0.0
require p = if p then possible else impossible

uniform_time_tree_pr age n_leaves tree = factor0 : [factor n | n <- [0 .. 2*n_leaves-2] ]
    where factor0 = doubleToLogDouble age ** intToDouble (2-n_leaves)
          time = node_time tree
          factor n = case parentNode tree n of Nothing -> possible
                                               Just b  -> require $ time n <= time p
                                                   where p = targetNode tree b

-- rooted_tree: force / modifiable / triggered_modifiable
force_rooted_tree rtree@(RootedTree unrooted_tree root_node _) = root_node `seq` force_tree unrooted_tree

-- leaves   nodes  branches
-- 1        1      0
-- 2        3      2
-- 3        5      4
-- 4        7      6
modifiable_rooted_tree modf (RootedTree tree root_node _) = add_root root_node $ Tree (listArray' nodes) (listArray' branches) n_nodes where
    n_nodes = numNodes tree
    n_leaves = (n_nodes + 1) `div` 2
    n_nodes = 2*n_leaves - 1
    n_branches = n_nodes - 1

    degree node | n_leaves == 1      = 0
                | node < n_leaves    = 1
                | node == root_node  = 2
                | node < n_nodes     = 3
                | otherwise          = error $ "modifiable_rooted_tree: unknown node"++show node

    reverse b = (b + n_branches) `mod` (2*n_branches)

    nodes    = [ mapn (degree node) modf (edgesOutOfNode tree node) | node <- xrange 0 n_nodes ]

    branches = [ (modf s, modf i, modf t, reverse b) | b <- xrange 0 (n_branches * 2),
                                                       let (s, i, t, _) = nodesForEdge tree b ]

-- our current modifiable tree structure requires the node to have a constrant degree.

triggered_modifiable_rooted_tree = triggered_modifiable_structure modifiable_rooted_tree force_rooted_tree

-- A uniform-ordered-history distribution would need to augment nodes with an Int order, instead of a Double order.

-- time_tree: force / modifiable / triggered_modifiable
force_time_tree (TimeTree rooted_tree times) = force_rooted_tree rooted_tree `seq` force_struct times

modifiable_time_tree modf (TimeTree rooted_tree' times') = TimeTree rooted_tree times where
    rooted_tree = modifiable_rooted_tree modf rooted_tree'
    times     = arrayMap modf times'

triggered_modifiable_time_tree = triggered_modifiable_structure modifiable_time_tree force_time_tree

uniform_time_tree_effect tree = tree `seq` sequence_ [ add_move $ slice_sample_real_random_variable (node_time!node) bnds
                                                     | node <- [0..numNodes tree], nodes /= root tree
                                                     ] where bnds = above 0.0

uniform_time_tree age n = Distribution (uniform_time_tree_pr age n)
                                       (no_quantile "uniform_time_tree")
                                       (RandomStructure uniform_time_tree_effect triggered_modifiable_time_tree (sample_uniform_time_tree age n))
                                       (TreeRange n)
