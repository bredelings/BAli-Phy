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

class IsForest t => IsTree t where
    type family Unrooted t
    type family Rooted t

    unroot :: t -> Unrooted t
    makeRooted :: t -> Rooted t

class (HasRoots t, IsTree t) => HasRoot t where
    root :: t -> NodeId

instance (HasRoots t, IsTree t) => HasRoot t where
    root tree = case roots tree of [r] -> r
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

instance IsForest Tree

instance IsTree Tree where
    type instance Unrooted Tree = Tree
    type instance Rooted Tree = WithRoots Tree

    unroot t = t
    makeRooted t = add_root root t where root = head $ (internal_nodes t ++ leaf_nodes t)

instance IsTree t => IsTree (WithRoots t) where
    type Unrooted (WithRoots t) = Unrooted t
    type Rooted   (WithRoots t) = WithRoots t

    unroot (WithRoots t _ _) = unroot t
    makeRooted t = t

instance IsTree t => IsTree (WithLabels t) where
    type Unrooted (WithLabels t) = WithLabels (Unrooted t)
    type Rooted (WithLabels t) = WithLabels (Rooted t)

    unroot (WithLabels t labels) = WithLabels (unroot t) labels
    makeRooted (WithLabels t labels) = WithLabels (makeRooted t) labels


instance IsTree t => IsTree (WithBranchLengths t) where
    type Unrooted (WithBranchLengths t) = WithBranchLengths (Unrooted t)
    type Rooted (WithBranchLengths t) = WithBranchLengths (Rooted t)

    unroot (WithBranchLengths t lengths) = WithBranchLengths (unroot t) lengths
    makeRooted (WithBranchLengths t lengths) = WithBranchLengths (makeRooted t) lengths

instance HasRoot t => IsTree (WithNodeTimes t) where
    type Unrooted (WithNodeTimes t) = WithBranchLengths (Unrooted t)
    type Rooted   (WithNodeTimes t) = WithNodeTimes (Rooted t)

    unroot tt@(WithNodeTimes t node_heights) = WithBranchLengths (unroot t) (getUEdgesSet tt & IntMap.fromSet (\b -> branch_length tt b))
    makeRooted (WithNodeTimes t node_heights) = WithNodeTimes (makeRooted t) node_heights


instance (IsTree t, HasNodeTimes t) => IsTree (WithBranchRates t) where
    type Unrooted (WithBranchRates t) = WithBranchLengths (Unrooted t)
    type Rooted (WithBranchRates t) = WithBranchRates (Rooted t)

    unroot tt@(WithBranchRates t _) = WithBranchLengths (unroot t) (getUEdgesSet tt & IntMap.fromSet (\b -> branch_length tt b))
    makeRooted (WithBranchRates t branchRates) = WithBranchRates (makeRooted t) branchRates


tree_from_edges nodes edges = Tree $ forest_from_edges nodes edges

allEdgesFromRoot tree = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree (root tree))

add_root r t = addRoots [r] t

