module Probability.Distribution.Tree.Coalescent where

import           Tree
import           Probability.Random
import           Probability.Distribution.Tree.Modifiable
import           Probability.Distribution.Tree.UniformTimeTree
import           Probability.Distribution.Tree.Util
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

getCoalescent t rate nodes = do
  let n = length nodes
      nChoose2  = fromIntegral $ (n*(n-1)) `div` 2
      totalRate = nChoose2 * rate
  coal <- remove 2 nodes
  case coal of Nothing -> return Nothing
               Just ([n1,n2],rest) -> do dt <- sample $ exponential (1/totalRate)
                                         return $ Just (t+dt,(n1,n2,rest))
getEvent [] = return Nothing
getEvent ((t,e):es) = return $ Just (t,(e,es))

getNextEvent Nothing        Nothing                  = Nothing
getNextEvent Nothing       (Just (t2,y))             = Just (t2, Right y)
getNextEvent (Just (t1,x)) Nothing                   = Just (t1, Left x)
getNextEvent (Just (t1,x)) (Just (t2,y)) | t1 < t2   = Just (t1, Left x)
                                         | otherwise = Just (t2, Right y)

sampleCoalescentTree theta leafTimes rateShifts = do

  let nLeaves = length leafTimes
      firstInternal = 1 + maximum [node | (time,node) <- leafTimes]
      nodes  =  [(time, Leaf node)      | (time, node) <- sortOn fst leafTimes]
      shifts =  [(time, RateShift rate) | (time, rate) <- sortOn fst rateShifts]
      events = merge (\x y -> fst x < fst y) nodes shifts

  let go :: Double -> Double -> Int -> [Int] -> [(Double,CoalEvent)] -> ([Int],[(Int,Int)],[(Int,Double)]) -> Random ([Int], [(Int,Int)], [(Int,Double)])
      go t1 rate nextNode activeNodes nextEvents (nodes, edges, nodeTimes) = do
         coal <- getCoalescent t1 rate activeNodes
         event <- getEvent nextEvents
         case getNextEvent coal event of
           Nothing -> return (nodes, edges, nodeTimes)
           Just (t2, Left (n1, n2, rest)) -> goCoal  rate nextNode             nextEvents  (nodes, edges, nodeTimes) (t2, (n1,n2,rest))
           Just (t2, Right (e,es))        -> goEvent rate nextNode activeNodes             (nodes, edges, nodeTimes) (t2, (e, es))

      goCoal rate (coalNode::Int) nextEvents (nodes,edges,nodeTimes) (t2,(n1,n2,rest)) =  go t2 rate nextNode' activeNodes' nextEvents (nodes', edges', nodeTimes')
          where nextNode' = coalNode+1
                activeNodes' = coalNode:rest
                nodes' = coalNode:nodes
                edges' = (n1,coalNode):(n2,coalNode):edges
                nodeTimes' = (coalNode,t2):nodeTimes

      goEvent rate nextNode activeNodes (nodes, edges, nodeTimes) (t2, (Leaf node      , events)) = go t2 rate  nextNode (node:activeNodes) events (node:nodes, edges, (node,t2):nodeTimes)
      goEvent rate nextNode activeNodes (nodes, edges, nodeTimes) (t2, (RateShift rate2, events)) = go t2 rate2 nextNode activeNodes        events (nodes, edges, nodeTimes)

  (nodes, edges, nodeTimes) <- go 0 (2/theta) firstInternal [] events ([], [], [])
  let root = head nodes
      topology = add_root root (tree_from_edges nodes edges)

  return (time_tree topology (IntMap.fromList nodeTimes))

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
