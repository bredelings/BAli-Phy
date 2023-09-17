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

{-
ISSUE: HasRoots basically puts a preferred direction on each edge of the graph.
       The graph is still an undirected graph though, because we don't have edges that don't have a reverseedge.
       For rooted trees, we don't take the reverse edges out of the graph.

       We could make a class called PreferredDirection that says whether each pair of reversed edges has one direction that
       is the real edge.  I guess you could then construct a directed graph where reversed edges don't actually exist.

       If we had actual directed graphs -- that don't necessarily have a reverseEdge function -- we might still want to
       talk about a reversed edge as a direction, so that we could talk about (for example) walking on the graph in the
       reverse direction of the edges.  I guess we could then have something like:

           data Direction e = Forward e | Reverse e
           reverse (Forward e) = Reverse e
           reverse (Reverse e) = Forward e
-}

{-
 For a directed graph, each node would have to have BOTH out-edges AND in-edges.

 For an undirected graph, we kind of have UEdges and Edges (aka directed edges).
 We could make a directed graph OUT OF an undirected graph.
 Maybe we could represent this as (Directed undirectedGraph) instead of constructing a new data structure with more edges.
 Its possible that we could make the EdgeId type for the directed version be the Direction UndirectedEdgeId, instead of an Int.
-}

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

instance HasRoots t => HasRoots (WithBranchLengths t) where
    roots (WithBranchLengths tree _) = roots tree
    isRoot (WithBranchLengths t _) node = isRoot t node
    away_from_root (WithBranchLengths tree _  ) b = away_from_root tree b

instance HasNodeTimes t => HasRoots (WithBranchRates t) where
    roots (WithBranchRates t _) = roots t
    isRoot (WithBranchRates t _) node = isRoot t node
    away_from_root (WithBranchRates tree _  ) b = away_from_root tree b

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

class HasRoots t => HasNodeTimes t where
    node_time :: t -> Int -> Double

instance HasRoots t => HasNodeTimes (WithNodeTimes t) where
    node_time (WithNodeTimes t hs) node = hs IntMap.! node

instance HasNodeTimes t => HasNodeTimes (WithLabels t) where
    node_time (WithLabels tt _) node = node_time tt node

instance HasNodeTimes t => HasNodeTimes (WithBranchRates t) where
    node_time (WithBranchRates tt _) node = node_time tt node

instance HasNodeTimes t => HasBranchRates (WithBranchRates t) where
    branch_rate (WithBranchRates _ rs) node = rs IntMap.! node

instance HasRoots t => HasBranchLengths (WithNodeTimes t) where
    branch_length tree b = branch_duration tree b

branch_duration t b = abs (node_time t source - node_time t target)
    where source = sourceNode t b
          target = targetNode t b

time_tree topology times = WithNodeTimes topology times

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

class HasNodeTimes t => HasBranchRates t where
    branch_rate :: t -> Int -> Double

instance (HasRoots t, HasLabels t) => HasLabels (WithNodeTimes t) where
    get_label (WithNodeTimes t _) node          = get_label t node
    get_labels (WithNodeTimes t _) = get_labels t
    relabel newLabels (WithNodeTimes t nodeHeights) = WithNodeTimes (relabel newLabels t) nodeHeights

instance (HasNodeTimes t, HasLabels t) => HasLabels (WithBranchRates t) where
    get_label (WithBranchRates t _) node      = get_label t node
    get_labels (WithBranchRates t _) = get_labels t
    relabel newLabels (WithBranchRates t branchRates) = WithBranchRates (relabel newLabels t) branchRates

instance HasNodeTimes t => HasBranchLengths (WithBranchRates t) where
    branch_length tree b = branch_duration tree b * branch_rate tree b

rate_time_tree time_tree rates = WithBranchRates time_tree rates

--

allEdgesAfterEdge tree b = b:concatMap (allEdgesAfterEdge tree) (edgesAfterEdge tree b)
allEdgesFromNode tree n = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree n)
allEdgesFromRoots forest = concat [concatMap (allEdgesAfterEdge forest) (edgesOutOfNode forest root) | root <- roots forest]

addRoots roots t = rt
    where check_away_from_root b = (sourceNode rt b `elem` roots) || (or $ fmap (away_from_root rt) (edgesBeforeEdge rt b))
          nb = numBranches t * 2
          rt = WithRoots t roots (getEdgesSet t & IntMap.fromSet check_away_from_root)

forest_from_edges nodes edges = Forest $ graph_from_edges nodes edges
