module Tree (module Tree, module Graph) where

import Graph
import Data.Foldable
import Data.Array
import Data.List (lookup)
import Data.Maybe (mapMaybes)

import Data.Maybe (fromJust)
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

-- NOTE: Data.Tree (Rose trees) and Data.Forest (collections of Data.Tree) exist, but are records, not classes.

class IsGraph f => IsForest f

class IsForest t => IsTree t where
    type family Unrooted t
    type family Rooted t

    unroot :: t -> Unrooted t
    makeRooted :: t -> Rooted t

class IsForest t => HasRoots t where
    isRoot :: t -> NodeId -> Bool
    roots :: t -> [NodeId]
    away_from_root :: t -> Int -> Bool

class (HasRoots t, IsTree t) => HasRoot t where
    root :: t -> NodeId

instance (HasRoots t, IsTree t) => HasRoot t where
    root tree = case roots tree of [r] -> r
                                   _ -> error "root: Tree has multiple roots!"

class HasRoot t => IsTimeTree t where
    node_time :: t -> Int -> Double

class IsTimeTree t => IsRateTimeTree t where
    branch_rate :: t -> Int -> Double

-- OK, so should we store attributes inside the tree?
-- 

data Forest = Forest Graph

data Tree   = Tree Forest

data WithRoots t = WithRoots t [NodeId] (IntMap Bool)

-- The array stores the node times
data WithNodeTimes t  = WithNodeTimes t (IntMap Double)

-- The array stores the branch rates
data WithBranchRates t = WithBranchRates t (IntMap Double)

instance IsGraph Forest where
    getNodesSet (Forest g) = getNodesSet g
    getEdgesSet (Forest g) = getEdgesSet g

    edgesOutOfNodeSet (Forest g) nodeId = edgesOutOfNodeSet g nodeId
    sourceNode (Forest g) edge = sourceNode g edge
    targetNode (Forest g) edge = targetNode g edge

    getNodeAttributes (Forest g) node = getNodeAttributes g node
    getEdgeAttributes (Forest g) edge = getEdgeAttributes g edge
    getAttributes (Forest g) = getAttributes g

instance IsGraph Tree where
    getNodesSet (Tree f) = getNodesSet f
    getEdgesSet (Tree f) = getEdgesSet f

    edgesOutOfNodeSet (Tree f) nodeId = edgesOutOfNodeSet f nodeId
    sourceNode (Tree f) edge = sourceNode f edge
    targetNode (Tree f) edge = targetNode f edge

    getNodeAttributes (Tree f) node = getNodeAttributes f node
    getEdgeAttributes (Tree f) edge = getEdgeAttributes f edge
    getAttributes (Tree f) = getAttributes f

instance IsForest Forest

instance IsForest Tree

instance IsTree Tree where
    type instance Unrooted Tree = Tree
    type instance Rooted Tree = WithRoots Tree

    unroot t = t
    makeRooted t = add_root root t where root = head $ (internal_nodes t ++ leaf_nodes t)

getNodeAttribute tree node key = lookup key ((\(Attributes as) -> as) $ getNodeAttributes tree node)
getEdgeAttribute tree edge key = lookup key ((\(Attributes as) -> as) $ getEdgeAttributes tree edge)
getTreeAttribute tree key = lookup key ((\(Attributes as) -> as) $ getAttributes tree)

edgeAttributes :: IsTree t => t -> Text -> ((Maybe (Maybe Text)) -> a) -> IntMap a
edgeAttributes tree key transform = fmap transform (getEdgesSet tree & IntMap.fromSet (\edge -> getEdgeAttribute tree edge key))

getAttribute key Nothing = error $ "No attribute '" ++ (T.unpack key) ++ "'"
getAttribute key (Just Nothing) = error $ "Attribute '" ++ T.unpack key ++ "' has no value"
getAttribute _   (Just (Just text)) = read (T.unpack text)

simpleEdgeAttributes tree key = edgeAttributes tree key (getAttribute key)

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

instance IsTree t => IsTree (WithRoots t) where
    type Unrooted (WithRoots t) = Unrooted t
    type Rooted   (WithRoots t) = WithRoots t

    unroot (WithRoots t _ _) = unroot t
    makeRooted t = t

instance IsForest f => IsForest (WithLabels f)

instance IsTree t => IsTree (WithLabels t) where
    type Unrooted (WithLabels t) = WithLabels (Unrooted t)
    type Rooted (WithLabels t) = WithLabels (Rooted t)

    unroot (WithLabels t labels) = WithLabels (unroot t) labels
    makeRooted (WithLabels t labels) = WithLabels (makeRooted t) labels


instance IsForest t => IsForest (WithBranchLengths t)

instance IsTree t => IsTree (WithBranchLengths t) where
    type Unrooted (WithBranchLengths t) = WithBranchLengths (Unrooted t)
    type Rooted (WithBranchLengths t) = WithBranchLengths (Rooted t)

    unroot (WithBranchLengths t lengths) = WithBranchLengths (unroot t) lengths
    makeRooted (WithBranchLengths t lengths) = WithBranchLengths (makeRooted t) lengths

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

instance HasRoot t => IsTree (WithNodeTimes t) where
    type Unrooted (WithNodeTimes t) = WithBranchLengths (Unrooted t)
    type Rooted   (WithNodeTimes t) = WithNodeTimes (Rooted t)

    unroot tt@(WithNodeTimes t node_heights) = WithBranchLengths (unroot t) (getUEdgesSet tt & IntMap.fromSet (\b -> branch_length tt b))
    makeRooted (WithNodeTimes t node_heights) = WithNodeTimes (makeRooted t) node_heights


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

instance IsTimeTree t => IsTree (WithBranchRates t) where
    type Unrooted (WithBranchRates t) = WithBranchLengths (Unrooted t)
    type Rooted (WithBranchRates t) = WithBranchRates (Rooted t)

    unroot tt@(WithBranchRates t _) = WithBranchLengths (unroot t) (getUEdgesSet tt & IntMap.fromSet (\b -> branch_length tt b))
    makeRooted (WithBranchRates t branchRates) = WithBranchRates (makeRooted t) branchRates


instance HasRoot t => IsTimeTree (WithNodeTimes t) where
    node_time (WithNodeTimes t hs) node = hs IntMap.! node

instance IsTimeTree t => IsTimeTree (WithLabels t) where
    node_time (WithLabels tt _) node = node_time tt node

instance IsTimeTree t => IsTimeTree (WithBranchRates t) where
    node_time (WithBranchRates tt _) node = node_time tt node

instance IsTimeTree t => IsRateTimeTree (WithBranchRates t) where
    branch_rate (WithBranchRates _ rs) node = rs IntMap.! node

branch_length_tree topology lengths = WithBranchLengths topology lengths

branch_lengths (WithBranchLengths _ ds) = ds

time_tree topology times = WithNodeTimes topology times

rate_time_tree time_tree rates = WithBranchRates time_tree rates

branch_duration t b = abs (node_time t source - node_time t target)
    where source = sourceNode t b
          target = targetNode t b

instance IsTree t => HasBranchLengths (WithBranchLengths t) where
    branch_length (WithBranchLengths tree ds) b = ds IntMap.! (undirectedName b)

instance IsTree t => CanModifyBranchLengths (WithBranchLengths t) where
    modifyBranchLengths f t@(WithBranchLengths tree ds) = WithBranchLengths tree (IntMap.fromSet f (IntMap.keysSet ds))

instance IsTimeTree t => HasBranchLengths (WithBranchRates t) where
    branch_length tree b = branch_duration tree b * branch_rate tree b

instance HasRoot t => HasBranchLengths (WithNodeTimes t) where
    branch_length tree b = branch_duration tree b

instance HasBranchLengths t => HasBranchLengths (WithLabels t) where
    branch_length (WithLabels tree _) b = branch_length tree b

instance CanModifyBranchLengths t => CanModifyBranchLengths (WithLabels t) where
    modifyBranchLengths f (WithLabels tree labels) = WithLabels (modifyBranchLengths f tree) labels

instance IsTree t => HasRoots (WithRoots t) where
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

instance IsTimeTree t => HasRoots (WithBranchRates t) where
    roots (WithBranchRates t _) = roots t
    isRoot (WithBranchRates t _) node = isRoot t node
    away_from_root (WithBranchRates tree _  ) b = away_from_root tree b

instance HasRoots t => HasRoots (WithBranchLengths t) where
    roots (WithBranchLengths tree _) = roots tree
    isRoot (WithBranchLengths t _) node = isRoot t node
    away_from_root (WithBranchLengths tree _  ) b = away_from_root tree b

-- Check for duplicate instances!

remove_root (WithRoots t _ _) = t
-- remove_root (WithLabels t labels) = WithLabels (remove_root t) labels

instance IsGraph t => HasLabels (WithLabels t) where
    get_label  (WithLabels _ labels) node = labels IntMap.! node
    get_labels (WithLabels _ labels) = labels
    relabel newLabels (WithLabels t _) = WithLabels t newLabels

instance HasLabels t => HasLabels (WithBranchLengths t) where
    get_label  (WithBranchLengths t _) node = get_label t node
    get_labels (WithBranchLengths t _) = get_labels t
    relabel newLabels (WithBranchLengths t lengths) = WithBranchLengths (relabel newLabels t) lengths

instance (HasRoot t, HasLabels t) => HasLabels (WithNodeTimes t) where
    get_label (WithNodeTimes t _) node          = get_label t node
    get_labels (WithNodeTimes t _) = get_labels t
    relabel newLabels (WithNodeTimes t nodeHeights) = WithNodeTimes (relabel newLabels t) nodeHeights

instance (IsTimeTree t, HasLabels t) => HasLabels (WithBranchRates t) where
    get_label (WithBranchRates t _) node      = get_label t node
    get_labels (WithBranchRates t _) = get_labels t
    relabel newLabels (WithBranchRates t branchRates) = WithBranchRates (relabel newLabels t) branchRates


toward_root rt b = not $ away_from_root rt b

branchToParent rtree node = find (toward_root rtree) (edgesOutOfNode rtree node)
branchFromParent rtree node = reverseEdge <$> branchToParent rtree node

parentNode rooted_tree n = case branchToParent rooted_tree n of Just b  -> Just $ targetNode rooted_tree b
                                                                Nothing -> Nothing

-- For numNodes, numBranches, edgesOutOfNode, and findEdge I'm currently using fake polymorphism
remove_element _ []     = [] -- no such element
remove_element 0 (x:xs) = xs
remove_element i (x:xs) = x:(remove_element (i-1) xs)

tree_from_edges nodes edges = Tree $ Forest $ graph_from_edges nodes edges

tree_length tree = sum [ branch_length tree b | b <- getUEdges tree ]

allEdgesAfterEdge tree b = b:concatMap (allEdgesAfterEdge tree) (edgesAfterEdge tree b)
allEdgesFromNode tree n = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree n)
allEdgesFromRoot tree = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree (root tree))

add_root r t = rt
     where check_away_from_root b = (sourceNode rt b == root rt) || (or $ fmap (away_from_root rt) (edgesBeforeEdge rt b))
           nb = numBranches t * 2
           rt = WithRoots t [r] (getEdgesSet t & IntMap.fromSet check_away_from_root)

