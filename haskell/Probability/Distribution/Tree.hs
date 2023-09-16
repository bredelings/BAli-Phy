module Probability.Distribution.Tree where

import           Tree
import           Probability.Random
import           Probability.Distribution.Uniform
import           Probability.Distribution.List
import           Probability.Distribution.Exponential
import           MCMC
import           Data.Array
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

xrange start end | start < end = start : xrange (start + 1) end
                 | otherwise   = []

pick_index 0 (h : t) = (h, t)
pick_index 0 []      = error "Trying to pick from empty list!"
pick_index i (h : t) = let (x, t2) = pick_index (i - 1) t in (x, h : t2)

remove_one []   = error "Cannot remove one from empty list"
remove_one list = do
    i <- sample $ uniform_int 0 (length list - 1)
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
sample_uniform_topology 1 = return $ Tree (IntMap.singleton 0 (Node 0 IntSet.empty)) (IntMap.empty) (IntMap.singleton 0 noAttributes) (IntMap.singleton 0 noAttributes) (Attributes [])
sample_uniform_topology n = do
    let num_nodes = 2 * n - 2
    edges <- uniform_topology_edges [0 .. n - 1] [n .. num_nodes - 1]
    return $ tree_from_edges [0..num_nodes-1] edges

{- Note: What the modifiable structure assumes.

   Our current modifiable tree structure requires that
   1. the name for the reverse edge doesn't change.
   2. the nodes and edges in the tree don't change.

   We could add `modf` in front of `r` to change (1).

   We could add `modf` in from of nodesMap and branchesMap to change (2).
   But we'd have to handle changes in the keysSet of node attributes (`na`) and edge attributes (`ea`).

   In both cases, adding more `modf` leads to:
   A. a slowdown
   B. a error saying that something isn't a modifiable value.
-}

modifiable_tree :: (forall a.a->a) -> TreeImp -> TreeImp
modifiable_tree modf tree@(Tree nodes0 branches0 na ea ta) = (Tree nodesMap branchesMap na ea ta) where
    nodesMap = fmap (\(Node node branches_out) -> Node node (modf branches_out)) nodes0
    branchesMap = fmap (\(Edge s t b) -> Edge (modf s) (modf t) b ) branches0

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

-- The *triggered* tree is lazy: when we access anything that is modifiable, it triggers all effects,
-- which includes forcing all the modifiables in the *untriggered* tree.

-- We don't want to force all fields of the tree when _any_ tree field is accessed, only when a _random_ field is accessed.
-- This is why triggered tree still uses 'tree' as input to 'modifiable_tree'.
triggered_modifiable_tree = triggered_modifiable_structure modifiable_tree

uniform_topology_effect tree = do
--  add_move $ walk_tree_sample_NNI_unsafe tree  -- probably we should ensure that the probability of the alignment is zero if pairwise alignments don't match?
  add_move $ walk_tree_sample_alignments tree  -- maybe this should be elsewhere?
                                               -- if this were elsewhere then we would have to walk the whole tree for each partition... which might not be terrible...
  add_move $ walk_tree_sample_NNI tree         -- does this handle situations with no data partitions?

uniform_labelled_topology taxa = do
  topology <- sample $ uniform_topology (length taxa)
  return $ add_labels (zip [0..] taxa) topology

add_alignment_moves tree = do
  SamplingRate 1 $ add_move $ walk_tree_sample_alignments tree
  SamplingRate 0.1 $ add_move $ realign_from_tips tree

add_SPR_moves tree = do
  SamplingRate 1 $ add_move $ sample_SPR_all tree
  SamplingRate 0.5 $ add_move $ sample_SPR_flat tree
  SamplingRate 0.5 $ add_move $ sample_SPR_nodes tree

add_topology_moves tree = do
  SamplingRate 1 $ add_SPR_moves tree
  SamplingRate 1 $ add_move $ walk_tree_sample_NNI_and_branch_lengths tree
  SamplingRate 2.0 $ add_move $ walk_tree_sample_NNI tree  -- if alignment is fixed this is really cheap -- increase weight?
  SamplingRate 0.5 $ add_move $ walk_tree_sample_NNI_and_A tree

add_length_moves tree = do
  SamplingRate 1 $ add_move $ walk_tree_sample_branch_lengths tree

add_tree_moves tree = do
  SamplingRate 1 $ add_topology_moves tree
  SamplingRate 1 $ add_length_moves tree

add_tree_alignment_moves tree = do
  SamplingRate 2 $ add_tree_moves tree
  SamplingRate 1 $ add_alignment_moves tree

uniform_labelled_tree taxa branch_lengths_dist = do
  -- These lines should be under SamplingRate 0.0 -- but then the polytomy trees won't work
  topology <- RanSamplingRate 0.0 $ uniform_labelled_topology taxa
  -- Q. How can we do walk_tree and then run the MCMC kernels that affect a given branch?
--  branch_lengths <- sample $ independent [branch_lengths_dist topology b | b <- [0..numBranches topology-1]]
  branch_lengths <- sample $ independent $ (getUEdgesSet topology & IntMap.fromSet (branch_lengths_dist topology))
  let tree = WithBranchLengths topology branch_lengths
  return tree `with_tk_effect` add_tree_alignment_moves

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
modifiable_rooted_tree :: (forall a.a -> a) -> WithRoots TreeImp -> WithRoots TreeImp
modifiable_rooted_tree modf (RootedTree tree [root_node] _) = add_root root_node $ modifiable_tree modf tree
-- Is it still true that we need the root node to have a constrant degree?

triggered_modifiable_rooted_tree = triggered_modifiable_structure modifiable_rooted_tree

-- A uniform-ordered-history distribution would need to augment nodes with an Int order, instead of a Double order.

-- maybe modf has type (forall a . a -> a)?
-- we should be able to apply it to both Int and Double...
modifiable_time_tree :: (forall a.a -> a) -> WithNodeTimes (WithRoots TreeImp) -> WithNodeTimes (WithRoots TreeImp)
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
data UniformTopology = UniformTopology Int

instance Dist UniformTopology where
    type Result UniformTopology = TreeImp
    dist_name _ = "uniform_topology"

instance HasAnnotatedPdf UniformTopology where
    annotated_densities (UniformTopology n) _ = return [uniform_topology_pr n]

instance Sampleable UniformTopology where
    sample dist@(UniformTopology n) = RanDistribution3 dist uniform_topology_effect triggered_modifiable_tree (sample_uniform_topology n)

uniform_topology n = UniformTopology n



-------------------------------------------------------------
data UniformTimeTree = UniformTimeTree Double Int

instance Dist UniformTimeTree where
    type Result UniformTimeTree = WithNodeTimes (WithRoots TreeImp)
    dist_name _ = "uniform_time_tree"

instance HasAnnotatedPdf UniformTimeTree where
    annotated_densities (UniformTimeTree age n) tree = return $ uniform_time_tree_pr age n tree

instance Sampleable UniformTimeTree where
    sample dist@(UniformTimeTree age n) = RanDistribution3 dist uniform_time_tree_effect triggered_modifiable_time_tree (sample_uniform_time_tree age n)

uniform_time_tree age n = UniformTimeTree age n

-------------------------------------------------------------
data CoalescentTree = CoalescentTree Double Int

instance Dist CoalescentTree where
    type Result CoalescentTree = WithNodeTimes (WithRoots TreeImp)
    dist_name _ = "uniform_time_tree"

instance HasAnnotatedPdf CoalescentTree where
    annotated_densities (CoalescentTree theta n) tree = return $ coalescent_tree_pr_factors theta n tree

instance Sampleable CoalescentTree where
    sample dist@(CoalescentTree theta n) = RanDistribution3 dist coalescent_tree_effect triggered_modifiable_time_tree (sample_coalescent_tree theta n)

coalescent_tree theta n = CoalescentTree theta n
