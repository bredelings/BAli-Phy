module Graph where

-- see fgl: Data.Graph - https://hackage.haskell.org/package/fgl
-- see Algebra.Graph - https://hackage.haskell.org/package/algebraic-graphs
-- see https://hackage.haskell.org/package/graphs

import Data.List (lookup)
import Data.Maybe (mapMaybes)

import Data.Maybe (fromJust)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as T

type NodeId = Int
type EdgeId = Int
type NodeIdSet = IntSet
type EdgeIdSet = IntSet

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

noAttributes = Attributes []
noAttributesOn set = set & IntMap.fromSet (\n -> noAttributes)

getNodeAttribute tree node key = lookup key ((\(Attributes as) -> as) $ getNodeAttributes tree node)
getEdgeAttribute tree edge key = lookup key ((\(Attributes as) -> as) $ getEdgeAttributes tree edge)
getTreeAttribute tree key = lookup key ((\(Attributes as) -> as) $ getAttributes tree)

--edgeAttributes :: IsGraph t => t -> Text -> ((Maybe (Maybe Text)) -> a) -> IntMap a
edgeAttributes tree key transform = fmap transform (getEdgesSet tree & IntMap.fromSet (\edge -> getEdgeAttribute tree edge key))

getAttribute key Nothing = error $ "No attribute '" ++ (T.unpack key) ++ "'"
getAttribute key (Just Nothing) = error $ "Attribute '" ++ T.unpack key ++ "' has no value"
getAttribute _   (Just (Just text)) = read (T.unpack text)

simpleEdgeAttributes tree key = edgeAttributes tree key (getAttribute key)


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

class IsGraph g where
    getNodesSet :: g -> NodeIdSet
    getEdgesSet :: g -> EdgeIdSet

    edgesOutOfNodeSet :: g -> NodeId -> EdgeIdSet
    sourceNode :: g -> EdgeId -> NodeId
    targetNode :: g -> EdgeId -> NodeId

    getNodeAttributes :: g -> NodeId -> Attributes
    getEdgeAttributes :: g -> EdgeId -> Attributes
    getAttributes :: g -> Attributes


class IsGraph g => IsDirectedGraph g where
    isForward :: g -> EdgeId -> Bool

outEdges g n = filter (isForward g) (edgesOutOfNode g n)
inEdges g n = map reverseEdge $ filter (not . isForward g) (edgesOutOfNode g n)

isSource g n = null (inEdges g n)
isSink g n = null (outEdges g n)

class IsDirectedGraph g => IsDirectedAcyclicGraph g

data Node = Node { node_name :: Int, node_out_edges:: IntSet}

instance Show Node where
    show (Node name out_edges) = "Node{node_name = " ++ show name ++ ", node_out_edges = " ++ show out_edges ++ "}"

-- ideally e_source_node and e_target_node would be of type Node,
--   and e_reverse would be of type Edge
data Edge = Edge { e_source_node, e_target_node, edge_name :: Int }

instance Show Edge where
    show (Edge source target name) = "Edge{e_source_node = " ++ show source ++ ", e_target_node = " ++ show target ++ ", edge_name = " ++ show name ++ "}"

data Graph = Graph (IntMap Node) (IntMap Edge) (IntMap Attributes) (IntMap Attributes) (Attributes)

{- ISSUE: How to handle directed graphs?

The underlying data structure represent out-edges.  We should be able to use this to represent
directed graphs if we don't insist that both e and -e are registered with the graph.  We could then
make a derived class IsUndirectedGraph with method reverseEdge.

-}

{- ISSUE: Should we store in-edges as well as out-edges? -}

{- ISSUE: Undirected graphs with a preferred direction aren't quite the same as directed graphs.

We currently derive IsDirectedGraph from IsGraph by adding an isForward attribute for an edge. -}

{- ISSUE: How to handle reverse edges on directed graphs?

SOLUTION: Make a "direction" that is either a forward or reverse edge:

       data Direction e = Forward e | Reverse e
       reverse (Forward e) = Reverse e
       reverse (Reverse e) = Forward e
-}


instance IsGraph Graph where
    getNodesSet (Graph nodesMap _  _ _ _)             = IntMap.keysSet nodesMap
    getEdgesSet (Graph _  edgesMap _ _ _)            = IntMap.keysSet edgesMap

    edgesOutOfNodeSet (Graph nodesMap _ _ _ _) nodeId = node_out_edges $ (nodesMap IntMap.! nodeId)
    sourceNode (Graph _ edgesMap _ _ _) edge = e_source_node $ (edgesMap IntMap.! edge)
    targetNode (Graph _ edgesMap _ _ _) edge = e_target_node $ (edgesMap IntMap.! edge)

    getNodeAttributes (Graph _ _ a _ _) node     = a IntMap.! node
    getEdgeAttributes (Graph _ _ _ a _) edge     = a IntMap.! edge
    getAttributes (Graph _ _ _ _ a)              = a

------------------ Derived Operations ------------
edgesTowardNodeArray t node = fmap reverseEdge $ edgesOutOfNodeArray t node
edgesTowardNode t node = fmap reverseEdge $ edgesOutOfNode t node
edgesTowardNodeSet t node = IntSet.mapNegate $ edgesOutOfNodeSet t node
edgeForNodes t (n1,n2) = fromJust $ find (\b -> targetNode t b == n2) (edgesOutOfNode t n1)

nodeDegree t n = IntSet.size (edgesOutOfNodeSet t n)
neighbors t n = fmap (targetNode t) (edgesOutOfNode t n)

edgesBeforeEdgeSet t b  = IntSet.mapNegate $ IntSet.delete b $ edgesOutOfNodeSet t node
    where node = sourceNode t b
edgesAfterEdgeSet t b = IntSet.delete (reverseEdge b) $ edgesOutOfNodeSet t node
    where node = targetNode t b

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

-- Given that this is a tree, would numNodes t - numBranches t + 2 work for n_leaves >=3?
numLeaves t = length $ leaf_nodes t

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

tree_length tree = sum [ branch_length tree b | b <- getUEdges tree ]

------------------ Branch Lengths ----------------

class IsGraph g => HasBranchLengths g where
    branch_length :: g -> Int -> Double

branchLengths g = branch_length g <$> getUEdges g

--   but could not do this for WithNodeTimes...
class HasBranchLengths g => CanModifyBranchLengths g where
    modifyBranchLengths :: (Int -> Double) -> g -> g

-- This seems to be unused in both Haskell and C++ code.
-- I guess it makes sense that you could construct a WithBranchLengths with arbitrary new branch lengths,
data WithBranchLengths t = WithBranchLengths t (IntMap Double)

instance IsGraph t => IsGraph (WithBranchLengths t) where
    getNodesSet (WithBranchLengths t _)             = getNodesSet t
    getEdgesSet (WithBranchLengths t _)             = getEdgesSet t

    edgesOutOfNodeSet (WithBranchLengths t _) nodeId    = edgesOutOfNodeSet t nodeId
    sourceNode (WithBranchLengths t _) edgeId           = sourceNode t edgeId
    targetNode (WithBranchLengths t _) edgeId           = targetNode t edgeId

    getNodeAttributes (WithBranchLengths t _) node     = getNodeAttributes t node
    getEdgeAttributes (WithBranchLengths t _) edge     = getEdgeAttributes t edge
    getAttributes (WithBranchLengths t _)              = getAttributes t

scale_branch_lengths factor (WithBranchLengths t ds) = (WithBranchLengths t ds')
    where ds' = fmap (factor*) ds

instance IsGraph t => HasBranchLengths (WithBranchLengths t) where
    branch_length (WithBranchLengths tree ds) b = ds IntMap.! (undirectedName b)

instance IsGraph t => CanModifyBranchLengths (WithBranchLengths t) where
    modifyBranchLengths f t@(WithBranchLengths tree ds) = WithBranchLengths tree (IntMap.fromSet f (IntMap.keysSet ds))

instance HasBranchLengths t => HasBranchLengths (WithLabels t) where
    branch_length (WithLabels tree _) b = branch_length tree b

instance CanModifyBranchLengths t => CanModifyBranchLengths (WithLabels t) where
    modifyBranchLengths f (WithLabels tree labels) = WithLabels (modifyBranchLengths f tree) labels

branch_lengths (WithBranchLengths _ ds) = ds

branch_length_tree topology lengths = WithBranchLengths topology lengths

------------------ Labels ----------------

class IsGraph g => HasLabels g where
    get_label :: g -> Int -> Maybe Text
    -- TODO: all_labels - a sorted list of labels that serves as a kind of taxon-map?
    -- this would map integers to labels, and labels to integers, even if get_label
    -- indexes on nodes...
    -- TODO: make the C++ code handle this...
    
    get_labels :: g -> IntMap (Maybe Text)
    relabel :: IntMap (Maybe Text) -> g -> g

data WithLabels t = WithLabels t (IntMap (Maybe Text))

instance IsGraph t => IsGraph (WithLabels t) where
    getNodesSet (WithLabels t _)                 = getNodesSet t
    getEdgesSet (WithLabels t _)                 = getEdgesSet t

    edgesOutOfNodeSet (WithLabels t _) nodeId    = edgesOutOfNodeSet t nodeId
    sourceNode (WithLabels t _) edgeId           = sourceNode t edgeId
    targetNode (WithLabels t _) edgeId           = targetNode t edgeId

    getNodeAttributes (WithLabels t _) node      = getNodeAttributes t node
    getEdgeAttributes (WithLabels t _) edge      = getEdgeAttributes t edge
    getAttributes (WithLabels t _)               = getAttributes t

instance IsGraph t => HasLabels (WithLabels t) where
    get_label  (WithLabels _ labels) node = labels IntMap.! node
    get_labels (WithLabels _ labels) = labels
    relabel newLabels (WithLabels t _) = WithLabels t newLabels

instance HasLabels t => HasLabels (WithBranchLengths t) where
    get_label  (WithBranchLengths t _) node = get_label t node
    get_labels (WithBranchLengths t _) = get_labels t
    relabel newLabels (WithBranchLengths t lengths) = WithBranchLengths (relabel newLabels t) lengths

instance IsDirectedGraph g => IsDirectedGraph (WithLabels g) where
    isForward (WithLabels g _) e = isForward g e

instance IsDirectedAcyclicGraph g => IsDirectedAcyclicGraph (WithLabels g)

add_labels labels t = WithLabels t (getNodesSet t & IntMap.fromSet (\node -> lookup node labels))

-- These two functions shouldn't go here -- but where should they go?
addInternalLabels tree = WithLabels tree newLabels where
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
    newLabels = getNodesSet t & IntMap.fromSet (\node -> if nodeDegree t node <= 1 then labels IntMap.! node else Nothing)

---------------------------- Creating a graph from a list of edges ---------------------------
graph_from_edges nodes edges = Graph nodesMap branchesMap (noAttributesOn nodesSet) (noAttributesOn branchesSet) noAttributes where

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

