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

-- FIXME!  This should be HasNodeTimes, and it should depend on HasRoots!
class HasRoots t => HasNodeTimes t where
    node_time :: t -> Int -> Double

class HasNodeTimes t => IsRateTimeTree t where
    branch_rate :: t -> Int -> Double

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


instance HasRoots t => HasNodeTimes (WithNodeTimes t) where
    node_time (WithNodeTimes t hs) node = hs IntMap.! node

instance HasNodeTimes t => HasNodeTimes (WithLabels t) where
    node_time (WithLabels tt _) node = node_time tt node

instance HasNodeTimes t => HasNodeTimes (WithBranchRates t) where
    node_time (WithBranchRates tt _) node = node_time tt node

instance HasNodeTimes t => IsRateTimeTree (WithBranchRates t) where
    branch_rate (WithBranchRates _ rs) node = rs IntMap.! node

branch_length_tree topology lengths = WithBranchLengths topology lengths

branch_lengths (WithBranchLengths _ ds) = ds

time_tree topology times = WithNodeTimes topology times

rate_time_tree time_tree rates = WithBranchRates time_tree rates

branch_duration t b = abs (node_time t source - node_time t target)
    where source = sourceNode t b
          target = targetNode t b

instance HasNodeTimes t => HasBranchLengths (WithBranchRates t) where
    branch_length tree b = branch_duration tree b * branch_rate tree b

instance HasRoot t => HasBranchLengths (WithNodeTimes t) where
    branch_length tree b = branch_duration tree b

instance HasBranchLengths t => HasBranchLengths (WithLabels t) where
    branch_length (WithLabels tree _) b = branch_length tree b

instance CanModifyBranchLengths t => CanModifyBranchLengths (WithLabels t) where
    modifyBranchLengths f (WithLabels tree labels) = WithLabels (modifyBranchLengths f tree) labels

instance HasNodeTimes t => HasRoots (WithBranchRates t) where
    roots (WithBranchRates t _) = roots t
    isRoot (WithBranchRates t _) node = isRoot t node
    away_from_root (WithBranchRates tree _  ) b = away_from_root tree b

instance HasRoots t => HasRoots (WithBranchLengths t) where
    roots (WithBranchLengths tree _) = roots tree
    isRoot (WithBranchLengths t _) node = isRoot t node
    away_from_root (WithBranchLengths tree _  ) b = away_from_root tree b

instance (HasRoot t, HasLabels t) => HasLabels (WithNodeTimes t) where
    get_label (WithNodeTimes t _) node          = get_label t node
    get_labels (WithNodeTimes t _) = get_labels t
    relabel newLabels (WithNodeTimes t nodeHeights) = WithNodeTimes (relabel newLabels t) nodeHeights

instance (HasNodeTimes t, HasLabels t) => HasLabels (WithBranchRates t) where
    get_label (WithBranchRates t _) node      = get_label t node
    get_labels (WithBranchRates t _) = get_labels t
    relabel newLabels (WithBranchRates t branchRates) = WithBranchRates (relabel newLabels t) branchRates


tree_from_edges nodes edges = Tree $ Forest $ graph_from_edges nodes edges

tree_length tree = sum [ branch_length tree b | b <- getUEdges tree ]

add_root r t = rt
     where check_away_from_root b = (sourceNode rt b == root rt) || (or $ fmap (away_from_root rt) (edgesBeforeEdge rt b))
           nb = numBranches t * 2
           rt = WithRoots t [r] (getEdgesSet t & IntMap.fromSet check_away_from_root)

allEdgesFromRoot tree = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree (root tree))
