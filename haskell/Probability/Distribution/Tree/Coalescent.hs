module Probability.Distribution.Tree.Coalescent where

import           Tree
import           Probability.Random
import           Probability.Distribution.Tree.Modifiable
import           Probability.Distribution.Tree.UniformTimeTree
import           Probability.Distribution.Tree.Util
import           Probability.Distribution.Exponential
import qualified Data.IntMap as IntMap
import           Data.Text (Text)
import           MCMC
import           Data.Array

type Time = Double

{- NOTE: Time scaling in the coalescent.

Time scale      Coalescent    Mutation     Generation       Time

generations       1/N          mu[g]         1              1/tau

coalescent         1          N*mu[g]        N              N/tau
                   1          theta/2        N              N/tau

phylogenetic    1/(N*mu[g])      1         1/mu[g]         1/mu[t]
                 2/(theta)       1         1/mu[g]         1/mu[t]

time units        tau/N        mu[t]        tau              1

Definitions:

  * mu[g] = mutation rate per generation (mu/gen)
  * mu[t] = mutatation rate per time unit = mu[g] * tau  (mu/time)
  * tau   = generation time (time/gen)
  * theta = 2*N*mu[g]  (individuals * mu / gen)

For a diploid model, replace N with 2N.

1. The natural scale is in generations.

2. The coalescent timescale allows one to derive the coalescent process as N->\infty.
   But it does creates complications when different populations have different sizes.

   Estimate: theta

3. The phylogenetic scale sets the rate of mutation to 1.
   This works well for multiple populations with different sizes.
   It also works well when we have no time information from serial samples / fossils.

   Estimate: theta

4. If we have serial samples / fossils, we need to use the time scale of the samples.
   We now need to estimates the mutation rate per time.

   Estimate: tau/N, mu[t]

-}


merge cmp [] ys = ys
merge cmp xs [] = xs
merge cmp xxs@(x:xs) yys@(y:ys) | cmp x y   = x:merge cmp xs yys
                                | otherwise = y:merge cmp xxs ys

data CoalEvent = Leaf Int | Internal Int | RateShift Double
nodeType tree node = if isLeafNode tree node then Leaf node else Internal node

instance Show CoalEvent where
    show (Leaf n ) = "Leaf " ++ show n
    show (Internal n ) = "Internal " ++ show n
    show (RateShift r ) = "RateShift " ++ show r

coalescentTreePrFactors ((t0,popSize0):rateShifts) tree = go t0 events 0 (1/popSize0) (parentBeforeChildPrs tree)
    where nodes = sortOn fst [ (nodeTime tree node, nodeType tree node) | node <- getNodes tree]
          shifts = [(time, RateShift (1/popSize)) | (time, popSize) <- rateShifts]
          events = merge (\x y -> fst x < fst y) nodes shifts
          go t1 []                  n rate factors = factors
          go t1 ((t2,event):events) n rate factors =
              case event of
                RateShift newRate -> go t2 events n     newRate (prNothing: factors)
                Leaf _            -> go t2 events (n+1) rate    (prNothing: factors)
                Internal _        -> go t2 events (n-1) rate    (prNothing * toLogDouble rate: factors)
                        -- the nChoose2 from the rate cancels with the one from the topology
              where nChoose2  = fromIntegral $ (n*(n-1)) `div` 2
                    prNothing = expToLogDouble $ (-rate * nChoose2 * (t2-t1))

-------------------------------------------------------------


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

sampleCoalescentTree leafTimes ((t0,popSize0):rateShifts) = do

  let nLeaves = length leafTimes
      firstInternal = 1 + maximum [node | (node, time) <- leafTimes]
      nodes  =  [(time, Leaf node)      | (node, time) <- sortOn snd leafTimes]
      shifts =  [(time, RateShift rate) | (time, rate) <- rateShifts]
      events = merge (\x y -> fst x < fst y) nodes shifts

  let go :: Double -> Double -> Int -> [Int] -> [(Double,CoalEvent)] -> ([Int],[(Int,Int)],[(Int,Double)]) -> Random ([Int], [(Int,Int)], [(Int,Double)])
      go t1 rate nextNode activeNodes nextEvents (nodes, edges, nodeTimes) = do
         coal <- getCoalescent t1 rate activeNodes
         event <- getEvent nextEvents
         case getNextEvent coal event of
           Nothing -> return (nodes, edges, nodeTimes)
           Just (t2, Left (n1, n2, rest)) -> goCoal  rate nextNode             nextEvents  (nodes, edges, nodeTimes) (t2, (n1,n2,rest))
           Just (t2, Right (event,events))        -> goEvent rate nextNode activeNodes             (nodes, edges, nodeTimes) (t2, (event, events))

      goCoal rate (coalNode::Int) nextEvents (nodes,edges,nodeTimes) (t2,(n1,n2,rest)) =  go t2 rate nextNode' activeNodes' nextEvents (nodes', edges', nodeTimes')
          where nextNode' = coalNode+1
                activeNodes' = coalNode:rest
                nodes' = coalNode:nodes
                edges' = (n1,coalNode):(n2,coalNode):edges
                nodeTimes' = (coalNode,t2):nodeTimes

      goEvent rate nextNode activeNodes (nodes, edges, nodeTimes) (t2, (Leaf node      , events)) = go t2 rate  nextNode (node:activeNodes) events (node:nodes, edges, (node,t2):nodeTimes)
      goEvent rate nextNode activeNodes (nodes, edges, nodeTimes) (t2, (RateShift rate2, events)) = go t2 rate2 nextNode activeNodes        events (nodes, edges, nodeTimes)

  (nodes, edges, nodeTimes) <- go t0 (1/popSize0) firstInternal [] events ([], [], [])
  let root = head nodes
      topology = addRoot root (treeFromEdges nodes edges)

  return (time_tree topology (IntMap.fromList nodeTimes))

-------------------------------------------------------------

-- FIXME: check that numLeaves tree is not changeable?
coalescentTreeEffect tree = do
  -- Resample all the node times, including the root...
  -- But what if some node times are fixed?
  -- FIXME: check that leaf times are fixed?
  -- sequence_ [ addMove 1 $ sliceSample (nodeTime tree node) (above 0) | node <- internalNodes tree]

  -- This allow attaching at the same level OR more rootward.
  -- FIXME: but it doesn't allow becoming the root!
  -- QUESTION: Could we slice sample the root location?
  -- QUESTION: Could we somehow propose a root location based on the likelihood/posterior?
  sequence_ [ addMove 1 $ metropolisHastings $ fnpr_unsafe_proposal tree node | node <- getNodes tree]

  -- Exchange sibling branches with children?
  addMove 1 $ walkTimeTreeSampleNNIandNodeTimes tree

-------------------------------------------------------------

data UnlabelledCoalescentTree = UnlabelledCoalescentTree [(Int,Time)] [(Time,Double)]

instance Dist UnlabelledCoalescentTree where
    type Result UnlabelledCoalescentTree = WithNodeTimes (WithRoots Tree)
    dist_name _ = "coalescentTree"

instance HasAnnotatedPdf UnlabelledCoalescentTree where
    annotated_densities (UnlabelledCoalescentTree leafTimes rateShifts) tree = return (coalescentTreePrFactors rateShifts tree, ())

instance Sampleable UnlabelledCoalescentTree where
    sample dist@(UnlabelledCoalescentTree leafTimes rateShifts) = RanDistribution3 dist coalescentTreeEffect triggeredModifiableTimeTree (sampleCoalescentTree leafTimes rateShifts)

unlabelledCoalescentTree leafTimes rateShifts = UnlabelledCoalescentTree leafTimes rateShifts

-------------------------------------------------------------

data CoalescentTree = CoalescentTree [(Text,Time)] [(Time,Double)]

instance Dist CoalescentTree where
    type Result CoalescentTree = WithLabels (WithNodeTimes (WithRoots Tree))
    dist_name _ = "CoalescentTree"

instance HasAnnotatedPdf CoalescentTree where
    annotated_densities (CoalescentTree taxonAges rateShifts) tree = return (coalescentTreePrFactors rateShifts tree, ())

instance Sampleable CoalescentTree where
    sample dist@(CoalescentTree taxonAges rateShifts) = addLabels leafIndices <$> (sample $ unlabelledCoalescentTree leafTimes rateShifts)
        where taxonAgeIndices = zip taxonAges [0..] 
              leafTimes = [(node,time) | ((name,time),node) <- taxonAgeIndices]
              leafIndices = [(node,name) | ((name,time),node) <- taxonAgeIndices]

coalescentTree taxonAges rateShifts = CoalescentTree taxonAges rateShifts

----------- Alternative coalescent sampling -----------------
-- This version references neighboring node structures directly, instead of just
-- recording their (integer) NAME.
-- * (GOOD) it doesn't require an array to map from integer names to nodes; this may avoid
--   invalidating all the CLs if the number of nodes
-- * (BAD)  it might be hard to create a wrapper tree that looks like this tree plus a
--           modifiation (e.g. deleting a tip).
data RootedTreeNode = RTNode { rTnodeName :: Int, rTnodeTime :: Time, rToutEdges :: [RootedTreeNode], rTinEdges :: [RootedTreeNode] }
-- ^ By allowing multiple in-edges, we can implement DAGs and Directed Forests, not just Trees.

-- When we don't know the node's parent yet, we store a function from parent(s) to node.
type NodeNoParent = [RootedTreeNode] -> RootedTreeNode

sampleCoalescentTree2 theta leafTimes rateShifts = do

  let nLeaves = length leafTimes
      firstInternal = 1 + maximum [node | (node, time) <- leafTimes]
      nodes  =  [(time, Leaf node)      | (node, time) <- sortOn fst leafTimes]
      shifts =  [(time, RateShift rate) | (time, rate) <- sortOn fst rateShifts]
      events = merge (\x y -> fst x < fst y) nodes shifts

  let go :: Double -> Double -> Int -> [NodeNoParent] -> [(Double, CoalEvent)] -> Random [NodeNoParent]
      go t1 rate nextNode activeSubtrees nextEvents = do
         coal <- getCoalescent t1 rate activeSubtrees
         event <- getEvent nextEvents
         case getNextEvent coal event of
           Nothing -> return activeSubtrees
           Just (t2, Left (n1, n2, rest)) -> goCoal  rate nextNode                nextEvents  (t2, (n1,n2,rest))
           Just (t2, Right (e,es))        -> goEvent rate nextNode activeSubtrees             (t2, (e, es))

      goCoal :: Double -> Int -> [(Double,CoalEvent)] -> (Double,(NodeNoParent,NodeNoParent,[NodeNoParent])) -> Random [NodeNoParent]
      goCoal rate coalNode nextEvents (t2,(n1,n2,rest)) =  go t2 rate nextNode' activeSubtrees' nextEvents
          where nextNode' = coalNode+1
                activeSubtrees' :: [NodeNoParent]
                activeSubtrees' = let node :: NodeNoParent
                                      node p = let node2 = RTNode coalNode t2 [n1 [node2], n2 [node2]] p in node2
                                  in node:rest

      goEvent rate nextNode activeSubtrees (t2, (Leaf node      , events)) = go t2 rate  nextNode activeSubtrees' events
          where activeSubtrees' = (RTNode node t2 []):activeSubtrees
      goEvent rate nextNode activeSubtrees (t2, (RateShift rate', events)) = go t2 rate' nextNode activeSubtrees  events

  trees <- go 0 (2/theta) firstInternal [] events

  case trees of [tree] -> return (tree [])
                _      -> error ("Sampling coalescence ended with " ++ show (length trees) ++ "subtrees!")

data RootedTree = RootedTree {
      getRoot :: RootedTreeNode,

      getNode :: (Array Int RootedTreeNode),
      getOrderedNode :: (Array Int Int)       -- cached the order of the nodes
}

instance Show RootedTree where
    show tree = show (getRoot tree)

instance Show RootedTreeNode where
    show (RTNode name time children maybeParent) = childrenStr ++ show name ++ branchStr
        where branchStr = case maybeParent of []                   -> ";"
                                              [RTNode _ pTime _ _] -> ":" ++ show (pTime - time)
                                              _                    -> error "Multiple parents -- not a tree!"
              childrenStr = case children of [] -> ""
                                             _  -> "(" ++ (intercalate "," (map show children)) ++ ")"
