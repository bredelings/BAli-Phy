module Forest (module Forest, module Graph) where

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



class IsGraph f => IsForest f

data Forest = Forest Graph

instance IsForest Forest

instance IsGraph Forest where
    getNodesSet (Forest g) = getNodesSet g
    getEdgesSet (Forest g) = getEdgesSet g

    edgesOutOfNodeSet (Forest g) nodeId = edgesOutOfNodeSet g nodeId
    sourceNode (Forest g) edge = sourceNode g edge
    targetNode (Forest g) edge = targetNode g edge

    getNodeAttributes (Forest g) node = getNodeAttributes g node
    getEdgeAttributes (Forest g) edge = getEdgeAttributes g edge
    getAttributes (Forest g) = getAttributes g

instance IsForest f => IsForest (WithLabels f)

instance IsForest t => IsForest (WithBranchLengths t)

-------------------------- Rooted forests-----------------------------------
data WithRoots t = WithRoots t [NodeId] (IntMap Bool)

class IsForest t => HasRoots t where
    isRoot :: t -> NodeId -> Bool
    roots :: t -> [NodeId]
    away_from_root :: t -> Int -> Bool

instance IsForest t => HasRoots (WithRoots t) where
    roots (WithRoots _ rs _) = rs
    isRoot (WithRoots _ rs _) node = node `elem` rs
    away_from_root (WithRoots t _ arr    ) b = arr IntMap.! b

instance HasRoots t => HasRoots (WithLabels t) where
    roots (WithLabels t _) = roots t
    isRoot (WithLabels t _) node = isRoot t node
    away_from_root (WithLabels t _      ) b = away_from_root t b

instance HasRoots t => HasRoots (WithNodeTimes t) where
    roots (WithNodeTimes t _)     = roots t
    isRoot (WithNodeTimes t _) node = isRoot t node
    away_from_root (WithNodeTimes   t _        ) b = away_from_root t b

instance IsGraph t => IsGraph (WithRoots t) where
    getNodesSet (WithRoots t _ _)                 = getNodesSet t
    getEdgesSet (WithRoots t _ _)                 = getEdgesSet t

    edgesOutOfNodeSet (WithRoots t _ _) nodeId       = edgesOutOfNodeSet t nodeId
    sourceNode (WithRoots t _ _) edgeId           = sourceNode t edgeId
    targetNode (WithRoots t _ _) edgeId           = targetNode t edgeId

    getNodeAttributes (WithRoots t _ _) node         = getNodeAttributes t node
    getEdgeAttributes (WithRoots t _ _) edge         = getEdgeAttributes t edge
    getAttributes (WithRoots t _ _)              = getAttributes t

instance IsForest t => IsForest (WithRoots t)

toward_root rt b = not $ away_from_root rt b

branchToParent rtree node = find (toward_root rtree) (edgesOutOfNode rtree node)
branchFromParent rtree node = reverseEdge <$> branchToParent rtree node

parentNode rooted_tree n = case branchToParent rooted_tree n of Just b  -> Just $ targetNode rooted_tree b
                                                                Nothing -> Nothing

-------------------------- Forests with node times--------------------------
-- The array stores the node times
data WithNodeTimes t  = WithNodeTimes t (IntMap Double)

instance IsGraph t => IsGraph (WithNodeTimes t) where
    getNodesSet (WithNodeTimes t _)                     = getNodesSet t
    getEdgesSet (WithNodeTimes t _)                     = getEdgesSet t

    edgesOutOfNodeSet (WithNodeTimes t _) nodeId    = edgesOutOfNodeSet t nodeId
    sourceNode (WithNodeTimes t _) edgeId           = sourceNode t edgeId
    targetNode (WithNodeTimes t _) edgeId           = targetNode t edgeId

    getNodeAttributes (WithNodeTimes t _) node     = getNodeAttributes t node
    getEdgeAttributes (WithNodeTimes t _) edge     = getEdgeAttributes t edge
    getAttributes (WithNodeTimes t _)              = getAttributes t

instance IsForest t => IsForest (WithNodeTimes t)

-------------------------- Forests with branch rates------------------------
-- The array stores the branch rates
data WithBranchRates t = WithBranchRates t (IntMap Double)

instance IsGraph t => IsGraph (WithBranchRates t) where
    getNodesSet (WithBranchRates t _)                 = getNodesSet t
    getEdgesSet (WithBranchRates t _)                 = getEdgesSet t

    edgesOutOfNodeSet (WithBranchRates t _) nodeId       = edgesOutOfNodeSet t nodeId
    sourceNode (WithBranchRates t _) edgeId           = sourceNode t edgeId
    targetNode (WithBranchRates t _) edgeId           = targetNode t edgeId

    getNodeAttributes (WithBranchRates t _) node         = getNodeAttributes t node
    getEdgeAttributes (WithBranchRates t _) edge         = getEdgeAttributes t edge
    getAttributes (WithBranchRates t _)                  = getAttributes t

instance IsForest t => IsForest (WithBranchRates t)

--

allEdgesAfterEdge tree b = b:concatMap (allEdgesAfterEdge tree) (edgesAfterEdge tree b)
allEdgesFromNode tree n = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree n)
allEdgesFromRoots forest = concat [concatMap (allEdgesAfterEdge forest) (edgesOutOfNode forest root) | root <- roots forest]

