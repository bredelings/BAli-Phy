module Probability.Distribution.Tree.Coalescent where

import           Tree
import           Probability.Random
import           Probability.Distribution.Tree.UniformTimeTree
import           Probability.Distribution.Tree.Modifiable
import           Probability.Distribution.Exponential
import qualified Data.IntMap as IntMap
import           MCMC

merge cmp [] ys = ys
merge cmp xs [] = xs
merge cmp xxs@(x:xs) yys@(y:ys) | cmp x y   = x:merge cmp xs yys
                                | otherwise = y:merge cmp xxs ys

data CoalEvent = Leaf Int | Internal Int | RateShift Double
nodeType tree node = if isLeafNode tree node then Leaf node else Internal node

coalescentTreePrFactors theta leafTimes tree rateShifts = go 0 events 0 (2/theta) 1: parentBeforeChildPrs nLeaves tree
    where nLeaves = length leafTimes
          nodes = sortOn fst [ (nodeTime tree node, nodeType tree node) | node <- [0..numNodes tree - 1]]
          shifts = [(time, RateShift rate) | (time, rate) <- sortOn fst rateShifts]
          events = merge (\x y -> fst x < fst y) nodes shifts
          go t1 []                  n rate pr = pr
          go t1 ((t2,event):events) n rate pr =
              case event of
                RateShift newRate -> go t2 events n     newRate pr'
                Leaf _            -> go t2 events (n+1) rate    pr'
                Internal _        -> go t2 events (n-1) rate    (pr' * toLogDouble rate)
                        -- the nChoose2 from the rate cancels with the one from the topology
              where nChoose2  = fromIntegral $ (n*(n-1)) `div` 2
                    totalRate = rate * nChoose2
                    prNothing = expToLogDouble $ (-totalRate * (t2-t1))
                    pr'       = pr * prNothing

-------------------------------------------------------------

-- We would sort and merge the Leaf and RateShift r events, and then
-- add the Internal events (effectively -- we would also need to
-- Or should it be (name,time) pairs?

sampleCoalescentTree theta leafTimes rateShifts = do
  let nodes  =  [(time, Leaf node) | (time, node) <- sortOn fst leafTimes]
      shifts =  [(time, RateShift rate) | (time, rate) <- sortOn fst rateShifts]
  
  let n_leaves = length leafTimes
  topology <- sample_uniform_ordered_tree n_leaves

  let rate = 2/theta
  dts <- sequence [ sample $ exponential (1 / (rate* n_choose_2) )| n <- reverse [2..n_leaves],
                                                                    let n_choose_2 = fromIntegral $ n*(n-1) `div` 2]
  let times = (replicate n_leaves 0) ++ (scanl1 (+) dts)
      nodeTimes = IntMap.fromList $ zip [0..] times
  return (time_tree topology nodeTimes)

-------------------------------------------------------------

-- FIXME: check that numLeaves tree is not changeable?
coalescentTreeEffect tree = do
  -- Resample all the node times, including the root...
  -- But what if some node times are fixed?
  -- FIXME: check that leaf times are fixed?
  sequence_ [ addMove 1 $ sliceSample (nodeTime tree node) (above 0) | node <- internalNodes tree]

  -- This allow attaching at the same level OR more rootward.
  -- FIXME: but it doesn't allow becoming the root!
  -- QUESTION: Could we slice sample the root location?
  -- QUESTION: Could we somehow propose a root location based on the likelihood/posterior?
  sequence_ [ addMove 1 $ metropolisHastings $ fnpr_unsafe_proposal tree node | node <- getNodes tree]

  -- Exchange sibling branches with children?
  sequence_ [ addMove 1 $ tnni_on_branch_unsafe tree branch | branch <- getEdges tree]

-------------------------------------------------------------

data CoalescentTree = CoalescentTree Double [(Double,Int)] [(Double,Double)]

instance Dist CoalescentTree where
    type Result CoalescentTree = WithNodeTimes (WithRoots Tree)
    dist_name _ = "coalescentTree"

instance HasAnnotatedPdf CoalescentTree where
    annotated_densities (CoalescentTree theta leafTimes rateShifts) tree = return (coalescentTreePrFactors theta leafTimes tree rateShifts, ())

instance Sampleable CoalescentTree where
    sample dist@(CoalescentTree theta leafTimes rateShifts) = RanDistribution3 dist coalescentTreeEffect triggeredModifiableTimeTree (sampleCoalescentTree theta leafTimes rateShifts)

coalescentTree theta leafTimes rateShifts = CoalescentTree theta leafTimes rateShifts
