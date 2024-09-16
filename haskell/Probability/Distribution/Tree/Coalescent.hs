module Probability.Distribution.Tree.Coalescent where

import           Tree
import           Probability.Random
import           Probability.Distribution.Tree.UniformTimeTree
import           Probability.Distribution.Tree.Modifiable
import           Probability.Distribution.Exponential
import qualified Data.IntMap as IntMap
import           MCMC

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

-------------------------------------------------------------

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

-- FIXME: check that numLeaves tree is not changeable?
coalescent_tree_effect tree = do
  -- Resample all the node times, including the root...
  -- But what if some node times are fixed?
  -- FIXME: check that leaf times are fixed?
  sequence_ [ addMove 1 $ sliceSample (node_time tree node) (above 0) | node <- internal_nodes tree]

  -- This allow attaching at the same level OR more rootward.
  -- FIXME: but it doesn't allow becoming the root!
  -- QUESTION: Could we slice sample the root location?
  -- QUESTION: Could we somehow propose a root location based on the likelihood/posterior?
  sequence_ [ addMove 1 $ metropolisHastings $ fnpr_unsafe_proposal tree node | node <- getNodes tree]

  -- Exchange sibling branches with children?
  sequence_ [ addMove 1 $ tnni_on_branch_unsafe tree branch | branch <- getEdges tree]

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
