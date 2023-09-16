module Tree where

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

data Attributes = Attributes [(Text,Maybe Text)]

(Attributes cs1) +:+ (Attributes cs2) = Attributes (cs1 ++ cs2)

instance Show Attributes where
    show (Attributes []) = ""
    show (Attributes cs) = "[&" ++ intercalate "," (fmap go cs)  ++ "]" where
                       go (k, Nothing) = T.unpack k
                       go (k, Just v)  = T.unpack k ++ "=" ++ T.unpack v

attributesText (Attributes []) = T.empty
attributesText (Attributes cs) = T.concat $ [ T.pack "[&" ] ++ intersperse (T.singleton ',') (fmap go cs) ++ [ T.pack "]" ]
    where go (k, Nothing) = k
          go (k, Just v)  = T.concat [k, T.singleton '=',v]


{-
 ISSUE: If we define graph operations in terms of node *ids*, then finding neighbors will depend on the id->Node map.
        If a node stores references to neighboring nodes themselves, then looking up the ids will not be necessary.
        Node ids are still necessary to determine if two nodes are the same or not.

 POSSIBLE SOLUTION: Make a type family Node t that abstracts over whether a "Node" is a number of something else.
        As long as we can find the neighboring nodes and such, then it doesn't matter what a "Node" is.
        Likewise for Branches.

  We would also need to have a function that gets the NodeID from a node.

  Also maybe we should just use Int's as IDs for now, since we aren't yet allowing the set of nodes/branches to change.
-}

{-
HasAttributes?
Graph => HasNodeAttributes?
Graph => HasEdgeAttributes?

Graph => NoCycles (Forest)
Graph => Connnected
Graph => HasLabels
Graph => HasBranchLengths

NoCycles => HasRoots { roots, isRoot, away_from_root }  -- You can't have a root on a graph with cycles, as edges could point both ways.
NoCycles, Connected => Tree

HasRoots, Tree => HasRoot

HasRoot => HasNodeTimes (IsTimeTree) -- Technically, we could have a DAG with node times, I think.
    The rule is probably that we aren't supposed to have a directed edge from a node an older node.

HasNodeTimes, Tree => IsTimeTree

IsTimeTree => IsRateTimeTree

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

type NodeId = Int
type EdgeId = Int
type NodeIdSet = IntSet
type EdgeIdSet = IntSet


class Forest f where
    getNodesSet :: f -> NodeIdSet
    getEdgesSet :: f -> EdgeIdSet

    edgesOutOfNodeSet :: f -> NodeId -> EdgeIdSet
    sourceNode :: f -> EdgeId -> NodeId
    targetNode :: f -> EdgeId -> NodeId

    getNodeAttributes :: f -> NodeId -> Attributes
    getEdgeAttributes :: f -> EdgeId -> Attributes
    getTreeAttributes :: f -> Attributes


class Forest t => Tree t where
    type family Unrooted t
    type family Rooted t

    unroot :: t -> Unrooted t
    makeRooted :: t -> Rooted t

getNodes t = t & getNodesSet & IntSet.toList
numNodes t = t & getNodesSet & IntSet.size

reverseEdge e = -e

isUEdge e = e > reverseEdge e

getEdges t  = getEdgesSet t & IntSet.toList
getUEdges t = [ e | e <- getEdges t, isUEdge e]
getUEdgesSet t = getUEdges t & IntSet.fromList
numBranches t = length $ getUEdges t

undirectedName e  = max e (reverseEdge e)

edgesOutOfNodeArray tree nodeIndex = IntSet.toArray $ edgesOutOfNodeSet tree nodeIndex
edgesOutOfNode tree nodeIndex = IntSet.toList $ edgesOutOfNodeSet tree nodeIndex

class Forest f => HasBranchLengths f where
    branch_length :: f -> Int -> Double

-- This seems to be unused in both Haskell and C++ code.
-- I guess it makes sense that you could construct a BranchLengthTree with arbitrary new branch lengths,
--   but could not do this for TimeTree...
class HasBranchLengths f => CanModifyBranchLengths f where
    modifyBranchLengths :: (Int -> Double) -> f -> f

class Forest t => HasRoots t where
    isRoot :: t -> NodeId -> Bool
    roots :: t -> [NodeId]
    away_from_root :: t -> Int -> Bool

class (HasRoots t, Tree t) => HasRoot t where
    root :: t -> NodeId

instance (HasRoots t, Tree t) => HasRoot t where
    root tree = case roots tree of [r] -> r
                                   _ -> error "root: Tree has multiple roots!"

class HasRoot t => IsTimeTree t where
    node_time :: t -> Int -> Double

class IsTimeTree t => IsRateTimeTree t where
    branch_rate :: t -> Int -> Double

class Forest f => HasLabels f where
    get_label :: f -> Int -> Maybe Text
    -- TODO: all_labels - a sorted list of labels that serves as a kind of taxon-map?
    -- this would map integers to labels, and labels to integers, even if get_label
    -- indexes on nodes...
    -- TODO: make the C++ code handle this...
    
    get_labels :: f -> IntMap (Maybe Text)
    relabel :: IntMap (Maybe Text) -> f -> f

-- OK, so should we store attributes inside the tree?
-- 

data Node = Node { node_name :: Int, node_out_edges:: IntSet}

instance Show Node where
    show (Node name out_edges) = "Node{node_name = " ++ show name ++ ", node_out_edges = " ++ show out_edges ++ "}"

-- ideally e_source_node and e_target_node would be of type Node,
--   and e_reverse would be of type Edge
data Edge = Edge { e_source_node, e_target_node, edge_name :: Int }

instance Show Edge where
    show (Edge source target name) = "Edge{e_source_node = " ++ show source ++ ", e_target_node = " ++ show target ++ ", edge_name = " ++ show name ++ "}"

data TreeImp = Tree (IntMap Node) (IntMap Edge) (IntMap Attributes) (IntMap Attributes) (Attributes)

data WithRoots t = RootedTree t [NodeId] (IntMap Bool)

data BranchLengthTreeImp t = BranchLengthTree t (IntMap Double)

data WithLabels t = LabelledTree t (IntMap (Maybe Text))

-- The array stores the node times
data TimeTreeImp t  = TimeTree t (IntMap Double)

-- The array stores the branch rates
data RateTimeTreeImp t = RateTimeTree t (IntMap Double)

instance Forest TreeImp where
    getNodesSet (Tree nodesMap _  _ _ _)             = IntMap.keysSet nodesMap
    getEdgesSet (Tree _  edgesMap _ _ _)            = IntMap.keysSet edgesMap

    edgesOutOfNodeSet (Tree nodesMap _ _ _ _) nodeId = node_out_edges $ (nodesMap IntMap.! nodeId)
    sourceNode (Tree _ edgesMap _ _ _) edge = e_source_node $ (edgesMap IntMap.! edge)
    targetNode (Tree _ edgesMap _ _ _) edge = e_target_node $ (edgesMap IntMap.! edge)

    getNodeAttributes (Tree _ _ a _ _) node         = a IntMap.! node
    getEdgeAttributes (Tree _ _ _ a _) edge         = a IntMap.! edge
    getTreeAttributes (Tree _ _ _ _ a)              = a

instance Tree TreeImp where
    type instance Unrooted TreeImp = TreeImp
    type instance Rooted TreeImp = WithRoots TreeImp

    unroot t = t
    makeRooted t = add_root root t where root = head $ (internal_nodes t ++ leaf_nodes t)

getNodeAttribute tree node key = lookup key ((\(Attributes as) -> as) $ getNodeAttributes tree node)
getEdgeAttribute tree edge key = lookup key ((\(Attributes as) -> as) $ getEdgeAttributes tree edge)
getTreeAttribute tree key = lookup key ((\(Attributes as) -> as) $ getTreeAttributes tree)

edgeAttributes :: Tree t => t -> Text -> ((Maybe (Maybe Text)) -> a) -> IntMap a
edgeAttributes tree key transform = fmap transform (getEdgesSet tree & IntMap.fromSet (\edge -> getEdgeAttribute tree edge key))

getAttribute key Nothing = error $ "No attribute '" ++ (T.unpack key) ++ "'"
getAttribute key (Just Nothing) = error $ "Attribute '" ++ T.unpack key ++ "' has no value"
getAttribute _   (Just (Just text)) = read (T.unpack text)

simpleEdgeAttributes tree key = edgeAttributes tree key (getAttribute key)

instance Forest t => Forest (WithRoots t) where
    getNodesSet (RootedTree t _ _)                 = getNodesSet t
    getEdgesSet (RootedTree t _ _)                 = getEdgesSet t

    edgesOutOfNodeSet (RootedTree t _ _) nodeId       = edgesOutOfNodeSet t nodeId
    sourceNode (RootedTree t _ _) edgeId           = sourceNode t edgeId
    targetNode (RootedTree t _ _) edgeId           = targetNode t edgeId

    getNodeAttributes (RootedTree t _ _) node         = getNodeAttributes t node
    getEdgeAttributes (RootedTree t _ _) edge         = getEdgeAttributes t edge
    getTreeAttributes (RootedTree t _ _)              = getTreeAttributes t

instance Tree t => Tree (WithRoots t) where
    type Unrooted (WithRoots t) = Unrooted t
    type Rooted   (WithRoots t) = WithRoots t

    unroot (RootedTree t _ _) = unroot t
    makeRooted t = t

instance Forest t => Forest (WithLabels t) where
    getNodesSet (LabelledTree t _)                 = getNodesSet t
    getEdgesSet (LabelledTree t _)                 = getEdgesSet t

    edgesOutOfNodeSet (LabelledTree t _) nodeId       = edgesOutOfNodeSet t nodeId
    sourceNode (LabelledTree t _) edgeId           = sourceNode t edgeId
    targetNode (LabelledTree t _) edgeId           = targetNode t edgeId

    getNodeAttributes (LabelledTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (LabelledTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (LabelledTree t _)              = getTreeAttributes t


instance Tree t => Tree (WithLabels t) where
    type Unrooted (WithLabels t) = WithLabels (Unrooted t)
    type Rooted (WithLabels t) = WithLabels (Rooted t)

    unroot (LabelledTree t labels) = LabelledTree (unroot t) labels
    makeRooted (LabelledTree t labels) = LabelledTree (makeRooted t) labels


instance Forest t => Forest (BranchLengthTreeImp t) where
    getNodesSet (BranchLengthTree t _)             = getNodesSet t
    getEdgesSet (BranchLengthTree t _)             = getEdgesSet t

    edgesOutOfNodeSet (BranchLengthTree t _) nodeId       = edgesOutOfNodeSet t nodeId
    sourceNode (BranchLengthTree t _) edgeId           = sourceNode t edgeId
    targetNode (BranchLengthTree t _) edgeId           = targetNode t edgeId

    getNodeAttributes (BranchLengthTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (BranchLengthTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (BranchLengthTree t _)              = getTreeAttributes t


instance Tree t => Tree (BranchLengthTreeImp t) where
    type Unrooted (BranchLengthTreeImp t) = BranchLengthTreeImp (Unrooted t)
    type Rooted (BranchLengthTreeImp t) = BranchLengthTreeImp (Rooted t)

    unroot (BranchLengthTree t lengths) = BranchLengthTree (unroot t) lengths
    makeRooted (BranchLengthTree t lengths) = BranchLengthTree (makeRooted t) lengths

instance HasRoots t => Forest (TimeTreeImp t) where
    getNodesSet (TimeTree t _)                     = getNodesSet t
    getEdgesSet (TimeTree t _)                     = getEdgesSet t

    edgesOutOfNodeSet (TimeTree t _) nodeId       = edgesOutOfNodeSet t nodeId
    sourceNode (TimeTree t _) edgeId           = sourceNode t edgeId
    targetNode (TimeTree t _) edgeId           = targetNode t edgeId

    getNodeAttributes (TimeTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (TimeTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (TimeTree t _)              = getTreeAttributes t

instance HasRoot t => Tree (TimeTreeImp t) where
    type Unrooted (TimeTreeImp t) = BranchLengthTreeImp (Unrooted t)
    type Rooted   (TimeTreeImp t) = TimeTreeImp (Rooted t)

    unroot tt@(TimeTree t node_heights) = BranchLengthTree (unroot t) (getUEdgesSet tt & IntMap.fromSet (\b -> branch_length tt b))
    makeRooted (TimeTree t node_heights) = TimeTree (makeRooted t) node_heights


instance IsTimeTree t => Forest (RateTimeTreeImp t) where
    getNodesSet (RateTimeTree t _)                 = getNodesSet t
    getEdgesSet (RateTimeTree t _)                 = getEdgesSet t

    edgesOutOfNodeSet (RateTimeTree t _) nodeId       = edgesOutOfNodeSet t nodeId
    sourceNode (RateTimeTree t _) edgeId           = sourceNode t edgeId
    targetNode (RateTimeTree t _) edgeId           = targetNode t edgeId

    getNodeAttributes (RateTimeTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (RateTimeTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (RateTimeTree t _)              = getTreeAttributes t

instance IsTimeTree t => Tree (RateTimeTreeImp t) where
    type Unrooted (RateTimeTreeImp t) = BranchLengthTreeImp (Unrooted t)
    type Rooted (RateTimeTreeImp t) = RateTimeTreeImp (Rooted t)

    unroot tt@(RateTimeTree t _) = BranchLengthTree (unroot t) (getUEdgesSet tt & IntMap.fromSet (\b -> branch_length tt b))
    makeRooted (RateTimeTree t branchRates) = RateTimeTree (makeRooted t) branchRates


instance HasRoot t => IsTimeTree (TimeTreeImp t) where
    node_time (TimeTree t hs) node = hs IntMap.! node

instance IsTimeTree t => IsTimeTree (WithLabels t) where
    node_time (LabelledTree tt _) node = node_time tt node

instance IsTimeTree t => IsTimeTree (RateTimeTreeImp t) where
    node_time (RateTimeTree tt _) node = node_time tt node

instance IsTimeTree t => IsRateTimeTree (RateTimeTreeImp t) where
    branch_rate (RateTimeTree _ rs) node = rs IntMap.! node

branch_length_tree topology lengths = BranchLengthTree topology lengths

branch_lengths (BranchLengthTree _ ds) = ds

time_tree topology times = TimeTree topology times

rate_time_tree time_tree rates = RateTimeTree time_tree rates

branch_duration t b = abs (node_time t source - node_time t target)
    where source = sourceNode t b
          target = targetNode t b

instance Tree t => HasBranchLengths (BranchLengthTreeImp t) where
    branch_length (BranchLengthTree tree ds) b = ds IntMap.! (undirectedName b)

instance Tree t => CanModifyBranchLengths (BranchLengthTreeImp t) where
    modifyBranchLengths f t@(BranchLengthTree tree ds) = BranchLengthTree tree (IntMap.fromSet f (IntMap.keysSet ds))

instance IsTimeTree t => HasBranchLengths (RateTimeTreeImp t) where
    branch_length tree b = branch_duration tree b * branch_rate tree b

instance HasRoot t => HasBranchLengths (TimeTreeImp t) where
    branch_length tree b = branch_duration tree b

instance HasBranchLengths t => HasBranchLengths (WithLabels t) where
    branch_length (LabelledTree tree _) b = branch_length tree b

instance CanModifyBranchLengths t => CanModifyBranchLengths (WithLabels t) where
    modifyBranchLengths f (LabelledTree tree labels) = LabelledTree (modifyBranchLengths f tree) labels

scale_branch_lengths factor (BranchLengthTree t ds) = (BranchLengthTree t ds')
    where ds' = fmap (factor*) ds

-- Given that this is a tree, would numNodes t - numBranches t + 2 work for n_leaves >=3?
numLeaves t = length $ leaf_nodes t


instance Tree t => HasRoots (WithRoots t) where
    roots (RootedTree _ rs _) = rs
    isRoot (RootedTree _ rs _) node = node `elem` rs
    away_from_root (RootedTree t _ arr    ) b = arr IntMap.! b

instance HasRoots t => HasRoots (WithLabels t) where
    roots (LabelledTree t _) = roots t
    isRoot (LabelledTree t _) node = isRoot t node
    away_from_root (LabelledTree t _      ) b = away_from_root t b

instance HasRoots t => HasRoots (TimeTreeImp t) where
    roots (TimeTree t _)     = roots t
    isRoot (TimeTree t _) node = isRoot t node
    away_from_root (TimeTree   t _        ) b = away_from_root t b

instance IsTimeTree t => HasRoots (RateTimeTreeImp t) where
    roots (RateTimeTree t _) = roots t
    isRoot (RateTimeTree t _) node = isRoot t node
    away_from_root (RateTimeTree tree _  ) b = away_from_root tree b

instance HasRoots t => HasRoots (BranchLengthTreeImp t) where
    roots (BranchLengthTree tree _) = roots tree
    isRoot (BranchLengthTree t _) node = isRoot t node
    away_from_root (BranchLengthTree tree _  ) b = away_from_root tree b

-- Check for duplicate instances!

remove_root (RootedTree t _ _) = t
-- remove_root (LabelledTree t labels) = LabelledTree (remove_root t) labels

instance Tree t => HasLabels (WithLabels t) where
    get_label  (LabelledTree _ labels) node = labels IntMap.! node
    get_labels (LabelledTree _ labels) = labels
    relabel newLabels (LabelledTree t _) = LabelledTree t newLabels

instance HasLabels t => HasLabels (BranchLengthTreeImp t) where
    get_label  (BranchLengthTree t _) node = get_label t node
    get_labels (BranchLengthTree t _) = get_labels t
    relabel newLabels (BranchLengthTree t lengths) = BranchLengthTree (relabel newLabels t) lengths

instance (HasRoot t, HasLabels t) => HasLabels (TimeTreeImp t) where
    get_label (TimeTree t _) node          = get_label t node
    get_labels (TimeTree t _) = get_labels t
    relabel newLabels (TimeTree t nodeHeights) = TimeTree (relabel newLabels t) nodeHeights

instance (IsTimeTree t, HasLabels t) => HasLabels (RateTimeTreeImp t) where
    get_label (RateTimeTree t _) node      = get_label t node
    get_labels (RateTimeTree t _) = get_labels t
    relabel newLabels (RateTimeTree t branchRates) = RateTimeTree (relabel newLabels t) branchRates


toward_root rt b = not $ away_from_root rt b

branchToParent rtree node = find (toward_root rtree) (edgesOutOfNode rtree node)
branchFromParent rtree node = reverseEdge <$> branchToParent rtree node

parentNode rooted_tree n = case branchToParent rooted_tree n of Just b  -> Just $ targetNode rooted_tree b
                                                                Nothing -> Nothing

-- For numNodes, numBranches, edgesOutOfNode, and findEdge I'm currently using fake polymorphism
edgesTowardNodeArray t node = fmap reverseEdge $ edgesOutOfNodeArray t node
edgesTowardNode t node = fmap reverseEdge $ edgesOutOfNode t node
edgeForNodes t (n1,n2) = fromJust $ find (\b -> targetNode t b == n2) (edgesOutOfNode t n1)
nodeDegree t n = IntSet.size (edgesOutOfNodeSet t n)
neighbors t n = fmap (targetNode t) (edgesOutOfNode t n)
edgesBeforeEdgeArray t b = fmap reverseEdge $ IntSet.toArray $ IntSet.delete b (edgesOutOfNodeSet t node)
    where node = sourceNode t b
edgesAfterEdgeArray t b = IntSet.toArray $ IntSet.delete (reverseEdge b) (edgesOutOfNodeSet t node)
    where node = targetNode t b
edgesBeforeEdge t b = fmap reverseEdge $ IntSet.toList $ IntSet.delete b (edgesOutOfNodeSet t node)
    where node = sourceNode t b
edgesAfterEdge t b = IntSet.toList $ IntSet.delete (reverseEdge b) (edgesOutOfNodeSet t node)
    where node = targetNode t b

is_leaf_node t n = (nodeDegree t n < 2)
is_internal_node t n = not $ is_leaf_node t n
is_internal_branch t b = is_internal_node t (sourceNode t b) && is_internal_node t (targetNode t b)
is_leaf_branch t b = not $ is_internal_branch t b

nodes t = getNodes t
leaf_nodes t = filter (is_leaf_node t) (nodes t)
internal_nodes t = filter (is_internal_node t) (nodes t)

remove_element _ []     = [] -- no such element
remove_element 0 (x:xs) = xs
remove_element i (x:xs) = x:(remove_element (i-1) xs)

noAttributes = Attributes []
noAttributesOn set = set & IntMap.fromSet (\n -> noAttributes)

tree_from_edges nodes edges = Tree nodesMap branchesMap (noAttributesOn nodesSet) (noAttributesOn branchesSet) noAttributes where

    num_nodes = length nodes

    -- FIX: this is a way to avoid depending changeables when |edges| is constant, but edges is changeable.
    num_branches = num_nodes - 1

    -- is this really how we want to name the branches?
    namedEdges = zip [1..] $ edges

    find_branch :: Int -> Maybe (Int,Int)
    find_branch b | b > 0     = fmap snd $ find (\e -> fst e == b) namedEdges
                  | otherwise = swap <$> (find_branch $ reverseEdge b)

    branchFrom n (b,(x,y)) | x == n    = Just b
                           | y == n    = Just (-b)
                           | otherwise = Nothing

    edgesFrom n = mapMaybes (branchFrom n) namedEdges

    nodesSet = IntSet.fromList nodes
    nodesMap = nodesSet & IntMap.fromSet (\n ->  Node n (IntSet.fromList $ edgesFrom n) )

    branchesSet = IntSet.fromList [1..num_branches] `IntSet.union` IntSet.fromList (map negate [1..num_branches])
    branchesMap = branchesSet & IntMap.fromSet (\b -> let Just (s,t) = find_branch b in Edge s t b)

tree_length tree = sum [ branch_length tree b | b <- getUEdges tree ]

allEdgesAfterEdge tree b = b:concatMap (allEdgesAfterEdge tree) (edgesAfterEdge tree b)
allEdgesFromNode tree n = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree n)
allEdgesFromRoot tree = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree (root tree))

add_labels labels t = LabelledTree t (getNodesSet t & IntMap.fromSet (\node -> lookup node labels))

add_root r t = rt
     where check_away_from_root b = (sourceNode rt b == root rt) || (or $ fmap (away_from_root rt) (edgesBeforeEdge rt b))
           nb = numBranches t * 2
           rt = RootedTree t [r] (getEdgesSet t & IntMap.fromSet check_away_from_root)

-- These two functions shouldn't go here -- but where should they go?
addInternalLabels tree = LabelledTree tree newLabels where
    oldLabels = get_labels tree
    newLabels = getNodesSet tree & IntMap.fromSet newLabel

    newLabel node = case (oldLabels IntMap.! node) of
                      Just label -> Just label
                      Nothing -> Just $ T.append (T.singleton 'A') (T.pack (show node))

add_ancestral_label node labels = case (labels IntMap.! node) of
                                    Just l -> l
                                    Nothing -> T.append (T.singleton 'A') (T.pack (show node))


dropInternalLabels t = relabel newLabels t where
    labels = get_labels t
    newLabels = getNodesSet t & IntMap.fromSet (\node -> if nodeDegree t node == 1 then labels IntMap.! node else Nothing)
