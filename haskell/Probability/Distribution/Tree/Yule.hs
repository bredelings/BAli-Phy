module Probability.Distribution.Tree.Yule where

import           Tree
import           Probability.Random
import           Probability.Distribution.Uniform
import           Probability.Distribution.Tree.Modifiable
import           Probability.Distribution.Tree.Util
import           Probability.Distribution.Exponential
import qualified Data.IntMap as IntMap
import           Data.Text (Text)
import           Data.Maybe (isJust, fromJust)
import           MCMC
import           Data.Array

yulePrFactors :: HasNodeTimes t => Int -> Rate -> t -> [LogDouble]
yulePrFactors n lambda tree = require (numLeaves tree == n)
                              : pow (toLogDouble lambda) (fromIntegral (n-2))
                              : [ expToLogDouble (-lambda * deltaT ) * require (deltaT >= 0) | node <- getNodes tree,
                                                                                               let parent = parentNode tree node,
                                                                                               isJust parent,
                                                                                               let deltaT = (nodeTime tree (fromJust parent) - nodeTime tree node)]

-------------------------------------------------------------
timesToAges tree = modifyNodeTimes tree (\t -> maxTime - t) where maxTime = maximum (nodeTimes tree)


sampleYule n lambda = do 

  let go :: Time -> Int -> [Int] -> ([Int],[(Int,Int)],[(Int,Double)]) -> Random ([Int], [(Int,Int)], [(Int,Double)])
      go t1 nextNode activeNodes (nodes, edges, nodeTimes) = do

              t2 <- (t1 +) <$> sample (exponential (1 / (lambda * (fromIntegral (length activeNodes)))))

              if length activeNodes == n then do
                  t2' <- sample $ uniform t1 t2
                  let nodeTimes' = [(node,t2') | node <- activeNodes] ++ nodeTimes
                  return (nodes, edges, nodeTimes')
              else do
                  (n0,rest) <- removeOne activeNodes
                  let n1 = nextNode
                      n2 = nextNode+1
                      nextNode' = nextNode + 2
                      nodes' = n1:n2:nodes
                      edges' = (n0,n1):(n0,n2):edges
                      nodeTimes' = (n0,t2):nodeTimes
                  go t2 nextNode'(n1:n2:rest) (nodes', edges', nodeTimes')

      t0 = 0
      nextNode0 = 3
      nodes0 = [0,1,2]
      edges0 = [(0,1),(0,2)]
      nodeTimes0 = [(0, t0)]
      activeNodes0 = [1,2]

  (nodes, edges, nodeTimes) <- go t0 nextNode0 activeNodes0 (nodes0, edges0, nodeTimes0)

  let root = 0
      topology = addRoot root (treeFromEdges nodes edges)
      timeTree = time_tree topology (IntMap.fromList nodeTimes)
      ageTree = timesToAges timeTree

  return ageTree

-------------------------------------------------------------

yuleEffect tree = do
  -- Resample all the node times, including the root...
  -- But what if some node times are fixed?
  -- FIXME: check that leaf times are fixed?
  -- sequence_ [ addMove 1 $ sliceSample (nodeTime tree node) (above 0) | node <- internalNodes tree]

  -- This allow attaching at the same level OR more rootward.
  -- FIXME: but it doesn't allow becoming the root!
  -- QUESTION: Could we slice sample the root location?
  -- QUESTION: Could we somehow propose a root location based on the likelihood/posterior?
  -- sequence_ [ addMove 1 $ metropolisHastings $ fnpr_unsafe_proposal tree node | node <- getNodes tree]

  -- Exchange sibling branches with children?
  addMove 1 $ walkTimeTreeSampleNNIandNodeTimes tree

-------------------------------------------------------------

data UnlabelledYule = UnlabelledYule Int Rate

instance Dist UnlabelledYule where
    type Result UnlabelledYule = WithNodeTimes (WithRoots Tree)
    dist_name _ = "yule"

instance HasAnnotatedPdf UnlabelledYule where
    annotated_densities (UnlabelledYule n lambda) tree = return (yulePrFactors n lambda tree, ())

instance Sampleable UnlabelledYule where
    sample dist@(UnlabelledYule n lambda) = RanDistribution3 dist yuleEffect triggeredModifiableTimeTree (sampleYule n lambda)

unlabelledYule n lambda = UnlabelledYule n lambda

-------------------------------------------------------------

data Yule = Yule [Text] Rate

instance Dist Yule where
    type Result Yule = WithLabels (WithNodeTimes (WithRoots Tree))
    dist_name _ = "Yule"

instance HasAnnotatedPdf Yule where
    annotated_densities (Yule taxa lambda) tree = return (yulePrFactors (length taxa) lambda tree, ())

instance Sampleable Yule where
    sample dist@(Yule taxa lambda) = do
                               tree <- sample $ unlabelledYule (length taxa) lambda
                               let leafIndices = zip (leafNodes tree) taxa
                               return $ addLabels leafIndices tree

yule taxa lambda = Yule taxa lambda
