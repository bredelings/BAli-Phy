module Tree (module Tree, module Forest, module Graph) where

import Forest
import Graph
import Data.Foldable
import Data.Array
import Data.List (lookup)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as T
import Control.DeepSeq

-- NOTE: Data.Tree (Rose trees) and Data.Forest (collections of Data.Tree) exist, but are records, not classes.

class IsForest t => IsTree t

class (HasRoots t, IsTree t) => HasRoot t where
    root :: t -> NodeId

instance (HasRoots t, IsTree t) => HasRoot t where
    root tree = case roots tree of [] -> error "root: Tree has no roots!"
                                   [r] -> r
                                   _ -> error "root: Tree has multiple roots!"

-- OK, so should we store attributes inside the tree?
-- 

data Tree   = Tree Forest

instance NFData Tree where
    rnf (Tree forest) = rnf forest

instance IsGraph Tree where
    getNodesSet (Tree f) = getNodesSet f
    getEdgesSet (Tree f) = getEdgesSet f

    edgesOutOfNodeSet (Tree f) nodeId = edgesOutOfNodeSet f nodeId
    sourceNode (Tree f) edge = sourceNode f edge
    targetNode (Tree f) edge = targetNode f edge

    getNodeAttributes (Tree f) node = getNodeAttributes f node
    getEdgeAttributes (Tree f) edge = getEdgeAttributes f edge
    getAttributes (Tree f) = getAttributes f

instance IsForest Tree where
    type instance Unrooted Tree = Tree
    type instance Rooted Tree = WithRoots Tree

    unroot t = t
    makeRooted t = addRoot root t where root = head $ (internalNodes t ++ leafNodes t)

instance IsTree Tree

instance IsTree t => IsTree (WithRoots t)

instance IsTree t => IsTree (WithLabels t)

instance IsTree t => IsTree (WithBranchLengths t)

instance HasRoot t => IsTree (WithNodeTimes t)

instance (IsTree t, HasNodeTimes t) => IsTree (WithBranchRates t)

treeFromEdges nodes edges = Tree $ forest_from_edges nodes edges

allEdgesFromRoot tree = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree (root tree))

-- addRoot :: IsTree t => NodeId -> t -> Rooted t
addRoot r t = addRoots [r] t

-- Should this go somewhere else?
weightedAverage weights values | length weights == length values = go weights values 0 0
                               | otherwise                       = error $ "weightedAverage: |weights| = " ++ show (length weights) ++ "  |values| = " ++ show (length values)
    where
      go (w:ws) (v:vs) top bot = go ws vs (top + (w * v)) (bot + w)
      go []     []     top bot = top / bot
