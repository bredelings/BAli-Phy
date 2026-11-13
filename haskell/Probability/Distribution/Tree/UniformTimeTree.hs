module Probability.Distribution.Tree.UniformTimeTree where

import Tree
import Probability.Random
import Probability.Distribution.List
import Probability.Distribution.Tree.Modifiable
import Probability.Distribution.Tree.Util
import Probability.Distribution.Uniform
import qualified Data.IntMap as IntMap
import MCMC
import Control.Monad.IO.Class (liftIO)
import Foreign.NativeVector (NativeVector)
import Data.Vector.Unboxed.Internal (intVectorFromNativeWithLength)
import qualified Data.Vector.Unboxed as U
import qualified Data.IntSet as IntSet

-- Uniform pairwise mergers give the ranked labelled topology used by Yule, birth-death,
-- and coalescent trees. Store each non-root child's upward edge as child+1 and its reverse
-- as -(child+1), so adjacency and direction need no repeated search through an edge list.
foreign import bpcall "TreeDist:sampleOrderedTree" sampleOrderedTreeNative :: Int -> IO (NativeVector Int)

-- Build each node and directed edge once from the native parent/child indices.
sampleUniformOrderedTree n = do
    native <- liftIO $ sampleOrderedTreeNative n
    let count = 2*n-1
        relatives = intVectorFromNativeWithLength (3*count) native
        parent v = relatives U.! (3*v)
        children v = if v < n then [] else [relatives U.! (3*v+1), relatives U.! (3*v+2)]
        nodes = IntSet.fromList [0..count-1]
        edges = IntSet.fromList ([1..count-1] ++ [1-count.. -1])
        outgoing v = (if v == count-1 then [] else [v+1]) ++ map (\c -> -(c+1)) (children v)
        nodeMap = IntMap.fromSet (\v -> Node v (IntSet.fromList (outgoing v))) nodes
        edgeMap = IntMap.fromSet (\b -> if b > 0 then Edge (b-1) (parent (b-1)) b
                                                else Edge (parent (-b-1)) (-b-1) b) edges
        labels = IntMap.fromSet (\_ -> Nothing) nodes
        graph = Graph nodeMap edgeMap labels (noAttributesOn nodes) (noAttributesOn edges) noAttributes
    return $ WithRoots (Tree (Forest graph)) [count-1] (IntMap.fromSet (<0) edges)

sampleUniformTimeTree age n = do
  topology <- sampleUniformOrderedTree n
  times <- sort <$> (sample $ iid (n-2) (uniform 0 age))
  let allTimes = replicate n 0 ++ times ++ [age]
      allNodeTimes = IntMap.fromList $ zip [0..] allTimes
  return $ time_tree topology allNodeTimes

uniformTimeTreePr :: (HasNodeTimes g, HasRoots g) => Double -> Int -> g -> [Log Double]
uniformTimeTreePr age nLeaves tree = factor0 : parentBeforeChildPrs tree
    where factor0 = toFloating age `pow` fromIntegral (2-nLeaves)

-- Add moves for non-root internal-node times.
-- FIXME: check that the leaves times are fixed?
-- FIXME: check that numLeaves tree is not changeable?
uniformTimeTreeEffect tree = sequence_ [ addMove 1 $ sliceSample (nodeTime tree node) (above 0.0)
                                       | node <- [numLeaves tree..numNodes tree - 1], node /= root tree
                                       ]

-- This doesn't handle serially-sampled tips... for that we would need to
-- * modify sample_uniform_ordered_tree
-- * pass in a list of (node,time) pairs.


-------------------------------------------------------------
data UniformTimeTree = UniformTimeTree Double Int

instance Dist UniformTimeTree where
    type Result UniformTimeTree = WithNodeTimes (WithRoots (Tree ()))
    distName _ = "uniformTimeTree"

instance HasPdf UniformTimeTree where
    densityWithProperties (UniformTimeTree age n) tree =
        (map fromLogDensity $ uniformTimeTreePr age n tree, ())

instance HasAnnotatedPdf UniformTimeTree

instance Sampleable UniformTimeTree where
    sample dist@(UniformTimeTree age n) = RanDistribution3 dist uniformTimeTreeEffect triggeredModifiableTimeTree (sampleUniformTimeTree age n)

uniformTimeTree age n = UniformTimeTree age n
