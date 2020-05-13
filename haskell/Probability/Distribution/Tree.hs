module Probability.Distribution.Tree where

import           Tree
import           Probability.Random
import           Probability.Distribution.Uniform

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

-- choose 2 leaves, connect them to an internal node, and put that internal node on the list of leaves
-- This is I think gives more weight to more balanced trees?
random_tree_edges [l1]     _        = return []
random_tree_edges [l1, l2] _        = return [(l1, l2)]
random_tree_edges leaves   (i : is) = do
    ([l1, l2], leaves') <- remove_n 2 leaves
    other_edges         <- random_tree_edges (i : leaves') is
    return $ [(l1, i), (l2, i)] ++ other_edges

-- Create a tree of size n-1, choose an edge at random, and insert the next leaf there.
uniform_topology_edges [l1]     _        = return []
uniform_topology_edges [l1, l2] _        = return [(l1, l2)]
uniform_topology_edges (l : ls) (i : is) = do
    es1           <- uniform_topology_edges ls is
    ((x, y), es2) <- remove_one es1
    return $ [(l, i), (x, i), (i, y)] ++ es2

-- We could rewrite uniform_topology_edges to automatically flip and sort the branches with leaf branches first.
random_tree 1 = return $ Tree (listArray' [[]]) (listArray' []) 1 0
random_tree n = do
    let num_nodes = 2 * n - 2
    edges <- uniform_topology_edges [0 .. n - 1] [n .. num_nodes - 1]
    -- This flipping is suppose flip edges from (internal,leaf) -> (leaf, internal)
    let maybe_flip (x, y) | (y < x)   = (y, x)
                          | otherwise = (x, y)
    -- Then the sorting is supposed order edges like (0,_), (1,_), (2,_)
    -- in order to assign leaf branches the names 0..n-1
    let sorted_edges     = sortOn fst $ map maybe_flip edges
    -- The number of edges should be 2*n-3, unchangably.
    let Tree es ns nn nb = tree_from_edges num_nodes sorted_edges
    return (Tree es ns nn (nn - 1))


force_tree tree@(Tree nodes branches n_nodes n_branches) = force_nodes `seq` force_branches where
    force_nodes    = force_struct $ listArray' [ force_list $ edgesOutOfNode tree node | node <- xrange 0 n_nodes ]
    force_branches = force_struct $ listArray' [ force_struct $ nodesForEdge tree b | b <- xrange 0 (n_branches * 2)]

-- leaves   nodes  branches
-- 1        1      0
-- 2        2      1
-- 3        4      3
-- 4        6      5
modifiable_cayley_tree n_leaves modf tree = Tree (listArray' nodes) (listArray' branches) n_nodes n_branches  where
    n_nodes | n_leaves == 1  = 1
            | otherwise      = 2*n_leaves - 2
    n_branches = n_nodes - 1
    degree node | n_leaves == 1   = 0
                | node < n_leaves = 1
                | otherwise       = 3
    nodes    = [ mapn (degree node) modf (edgesOutOfNode tree node) | node <- xrange 0 n_nodes ]
    branches = [ (modf s, modf i, modf t, (b + n_branches) `mod` (2*n_branches)) | b <- xrange 0 (n_branches * 2), let (s, i, t, _) = nodesForEdge tree b ]

uniform_topology_pr 1 = doubleToLogDouble 1.0
uniform_topology_pr 2 = doubleToLogDouble 1.0
uniform_topology_pr n = uniform_topology_pr (n - 1) / (doubleToLogDouble $ intToDouble $ 2 * n - 5)

-- The *triggered* tree is lazy: when we access anything that is modifiable, it triggers all effects,
-- which includes forcing all the modifiables in the *untriggered* tree.

-- We don't want to force all fields of the tree when _any_ tree field is accessed, only when a _random_ field is accessed.
-- This is why triggered tree still uses 'tree' as input to 'modifiable_tree'.
triggered_modifiable_tree n value effect = (raw_tree, triggered_tree) where
    raw_tree       = modifiable_cayley_tree n modifiable value
    effect'        = force_tree raw_tree `seq` effect
    triggered_tree = modifiable_cayley_tree n (effect' `seq`) raw_tree

uniform_topology n = Distribution (\tree -> [uniform_topology_pr n])
                                  (no_quantile "uniform_topology")
                                  (RandomStructure do_nothing (triggered_modifiable_tree n) (random_tree n))
                                  (TreeRange n)

uniform_labelled_topology taxa = do
  topology <- uniform_topology (length taxa)
  return $ add_labels topology taxa
