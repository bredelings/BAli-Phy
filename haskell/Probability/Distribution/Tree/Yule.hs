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
                              : [ expToLogDouble (-lambda * deltaT ) * require (deltaT >= 0)
                                      | node <- getNodes tree,
                                        let parent = parentNode tree node,
                                        isJust parent,
                                        let deltaT = (nodeTime tree (fromJust parent) - nodeTime tree node)
                                ]

-------------------------------------------------------------
timesToAges tree = modifyNodeTimes tree (maxTime -) where maxTime = maximum (nodeTimes tree)

-- This way of constructing the tree -- from the root to the tips, ensures that the tip
--    labels will look like 1,(3,4) or (3,4),2 when we have 3 leaves.
-- When a node i splits into two new nodes, we need to use i as the internal node name
--    because we already have a branch connected to it.
-- We could additionally sample integer labels [0..n-1] and randomly assign them to the tips.
--    When we hit the present we can shuffle the existing node numbers to assign labels [0..n]
-- Alternatively, we could also try arrange that the labels for each tip are actually a function
--    that passes additional labels to the ancestor.

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
{-
   We need the random shuffling to be performed in the IO monad (which doesn't remember anything)
   during the initial sample.

   We do NOT want the shuffle to be performed in the Random monad, because we don't want its
   random choices to be remembered and then resampled during MCMC.
 -}

sampleLabeledYule labels lambda = do
  tree <- sample $ unlabelledYule (length labels) lambda
  leafIndices <- zip (leafNodes tree) <$> shuffle labels
  return $ addLabels leafIndices tree

data Yule = Yule [Text] Rate

instance Dist Yule where
    type Result Yule = WithLabels (WithNodeTimes (WithRoots Tree))
    dist_name _ = "Yule"

instance HasAnnotatedPdf Yule where
    annotated_densities (Yule taxa lambda) tree = return (yulePrFactors (length taxa) lambda tree, ())

instance Sampleable Yule where
    sample dist@(Yule taxa lambda) = RanDistribution3 dist yuleEffect triggeredModifiableLabeledTimeTree (sampleLabeledYule taxa lambda)

yule taxa lambda = Yule taxa lambda
