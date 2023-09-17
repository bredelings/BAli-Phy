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
    makeRooted t = add_root root t where root = head $ (internal_nodes t ++ leaf_nodes t)

instance IsTree Tree

instance IsTree t => IsTree (WithRoots t)

instance IsTree t => IsTree (WithLabels t)

instance IsTree t => IsTree (WithBranchLengths t)

instance HasRoot t => IsTree (WithNodeTimes t)

instance (IsTree t, HasNodeTimes t) => IsTree (WithBranchRates t)

tree_from_edges nodes edges = Tree $ forest_from_edges nodes edges

allEdgesFromRoot tree = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree (root tree))

-- add_root :: IsTree t => NodeId -> t -> Rooted t
add_root r t = addRoots [r] t

