module Graph where

import Control.DeepSeq
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

instance NFData Attributes where
    rnf (Attributes as) = rnf as

(Attributes cs1) +:+ (Attributes cs2) = Attributes (cs1 ++ cs2)

emptyAttributes (Attributes l) = null l

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

    type family LabelType g
    getLabel :: g -> Int -> Maybe (LabelType g)
    -- TODO: all_labels - a sorted list of labels that serves as a kind of taxon-map?
    -- this would map integers to labels, and labels to integers, even if get_label
    -- indexes on nodes...
    -- TODO: make the C++ code handle this...

    getLabels :: g -> IntMap (Maybe (LabelType g))
    relabel :: IntMap (Maybe (LabelType g)) -> g -> g

class IsGraph g => IsDirectedGraph g where
    isForward :: g -> EdgeId -> Bool

outEdges g n = filter (isForward g) (edgesOutOfNode g n)
inEdges g n = map reverseEdge $ filter (not . isForward g) (edgesOutOfNode g n)

isSource g n = null (inEdges g n)
isSink g n = null (outEdges g n)

class IsDirectedGraph g => IsDirectedAcyclicGraph g

data Node = Node { nodeName :: Int, nodeOutEdges:: IntSet}

instance Show Node where
    show (Node name outEdges) = "Node{nodeName = " ++ show name ++ ", nodeOutEdges = " ++ show outEdges ++ "}"

-- ideally eSourceNode and eTargetNode would be of type Node,
--   and e_reverse would be of type Edge
data Edge = Edge { eSourceNode, eTargetNode, edgeName :: Int }

instance Show Edge where
    show (Edge source target name) = "Edge{eSourceNode = " ++ show source ++ ", eTargetNode = " ++ show target ++ ", edgeName = " ++ show name ++ "}"

data Graph l = Graph {
      graphNodes :: IntMap Node,
      graphEdges :: IntMap Edge,
      graphLabels :: IntMap (Maybe l),
      graphNodeAttributes :: IntMap Attributes,
      graphEdgeAttributes :: IntMap Attributes,
      graphAttributes :: Attributes
    }

instance NFData Node where
    rnf (Node x y) = x `seq` y `seq` ()

instance NFData Edge where
    rnf (Edge s t n) = s `seq` t `seq` n `seq` ()

--FIXME: `instance NFData Graph` does not complain, but makes no sense!
instance NFData l => NFData (Graph l) where
    rnf (Graph nodes edges labels nodeAttr edgeAttr graphAttr) = rnf nodes `seq` rnf edges `seq` rnf labels `seq` rnf nodeAttr `seq` rnf edgeAttr `seq` rnf graphAttr `seq` ()

{- ISSUE: How to handle directed graphs?

The underlying data structure represent out-edges.  We should be able to use this to represent
directed graphs if we don't insist that both e and -e are registered with the graph.  We could then
make a derived class IsUndirectedGraph with method reverseEdge.

-}

{- QUESTION: How to handle "partially directed" graphs?

For Phylogenetic networks, we have nodes with a edge groups [(source,target) | target <- sources],
but only one source can be active at a given time.  So if we choose a source for each edge, we
yet again get a normal graph.

In probabilistic terms, each target is associated with a probability.

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


instance IsGraph (Graph l) where
    getNodesSet (Graph nodesMap _ _  _ _ _)            = IntMap.keysSet nodesMap
    getEdgesSet (Graph _  edgesMap _ _ _ _)            = IntMap.keysSet edgesMap

    edgesOutOfNodeSet (Graph nodesMap _ _ _ _ _) nodeId = nodeOutEdges $ (nodesMap IntMap.! nodeId)
    sourceNode (Graph _ edgesMap _ _ _ _) edge = eSourceNode $ (edgesMap IntMap.! edge)
    targetNode (Graph _ edgesMap _ _ _ _) edge = eTargetNode $ (edgesMap IntMap.! edge)

    getNodeAttributes (Graph _ _ _ a _ _) node     = a IntMap.! node
    getEdgeAttributes (Graph _ _ _ _ a _) edge     = a IntMap.! edge
    getAttributes (Graph _ _ _ _ _ a)              = a


    -- What this does NOT say how to do is to fmap the labels to a new type
    -- labelMap display graph
    type instance LabelType (Graph l) = l
    getLabel graph node = (graphLabels graph) IntMap.! node
    getLabels graph = graphLabels graph
    relabel newLabels (Graph nodes edges _ na ea a) = Graph nodes edges newLabels na ea a

------------------ Derived Operations ------------
edgesTowardNodeSet t node = reverseEdgesSet $ edgesOutOfNodeSet t node
edgesTowardNodeArray t node = IntSet.toArray $ edgesTowardNodeSet t node
edgesTowardNode t node = IntSet.toList $ edgesTowardNodeSet t node

edgeForNodes t (n1,n2) = fromJust $ find (\b -> targetNode t b == n2) (edgesOutOfNode t n1)

nodeDegree t n = IntSet.size (edgesOutOfNodeSet t n)
neighbors t n = fmap (targetNode t) (edgesOutOfNode t n)

edgesBeforeEdgeSet t b  = reverseEdgesSet $ IntSet.delete b $ edgesOutOfNodeSet t node
    where node = sourceNode t b
edgesAfterEdgeSet t b = IntSet.delete (reverseEdge b) $ edgesOutOfNodeSet t node
    where node = targetNode t b

edgesBeforeEdgeArray t b = IntSet.toArray $ edgesBeforeEdgeSet t b
edgesAfterEdgeArray t b = IntSet.toArray $ edgesAfterEdgeSet t b
edgesBeforeEdge t b = IntSet.toList $ edgesBeforeEdgeSet t b
edgesAfterEdge t b = IntSet.toList $ edgesAfterEdgeSet t b

isLeafNode t n = (nodeDegree t n < 2)
isInternalNode t n = not $ isLeafNode t n
isInternalBranch t b = isInternalNode t (sourceNode t b) && isInternalNode t (targetNode t b)
isLeafBranch t b = not $ isInternalBranch t b

nodes t = getNodes t
leafNodes t = filter (isLeafNode t) (nodes t)
internalNodes t = filter (isInternalNode t) (nodes t)

-- Given that this is a tree, would numNodes t - numBranches t + 2 work for n_leaves >=3?
numLeaves t = length $ leafNodes t

getNodes t = t & getNodesSet & IntSet.toList
numNodes t = t & getNodesSet & IntSet.size

reverseEdge e = -e

reverseEdges = map reverseEdge
reverseEdgesSet = IntSet.mapNegate
--reverseEdgesArray :: Array Int -> Array Int
reverseEdgesArray = fmap reverseEdge

isUEdge e = e > reverseEdge e

getEdges t  = getEdgesSet t & IntSet.toList
getUEdges t = [ e | e <- getEdges t, isUEdge e]
getUEdgesSet t = getUEdges t & IntSet.fromList
numBranches t = length $ getUEdges t

undirectedName e  = max e (reverseEdge e)

edgesOutOfNodeArray tree nodeIndex = IntSet.toArray $ edgesOutOfNodeSet tree nodeIndex
edgesOutOfNode tree nodeIndex = IntSet.toList $ edgesOutOfNodeSet tree nodeIndex

treeLength tree = sum [ branchLength tree b | b <- getUEdges tree ]

------------------ Branch Lengths ----------------

class IsGraph g => HasBranchLengths g where
    branchLength :: g -> Int -> Double

branchLengths g = branchLength g <$> getUEdges g
branchLengthsSet g = getUEdgesSet g & IntMap.fromSet (branchLength g)

--   but could not do this for WithNodeTimes...
--   QUESTION: should this just be (Double->Double)?  Or (Int->Double->Double)?
class HasBranchLengths g => CanModifyBranchLengths g where
    modifyBranchLengths :: (Int -> Double) -> g -> g

-- This seems to be unused in both Haskell and C++ code.
-- I guess it makes sense that you could construct a WithBranchLengths with arbitrary new branch lengths,
data WithBranchLengths t = WithBranchLengths t (IntMap Double)

instance NFData t => NFData (WithBranchLengths t) where
    rnf (WithBranchLengths tree lengths) = rnf tree `seq` rnf lengths

instance IsGraph t => IsGraph (WithBranchLengths t) where
    getNodesSet (WithBranchLengths t _)             = getNodesSet t
    getEdgesSet (WithBranchLengths t _)             = getEdgesSet t

    edgesOutOfNodeSet (WithBranchLengths t _) nodeId    = edgesOutOfNodeSet t nodeId
    sourceNode (WithBranchLengths t _) edgeId           = sourceNode t edgeId
    targetNode (WithBranchLengths t _) edgeId           = targetNode t edgeId

    getNodeAttributes (WithBranchLengths t _) node     = getNodeAttributes t node
    getEdgeAttributes (WithBranchLengths t _) edge     = getEdgeAttributes t edge
    getAttributes (WithBranchLengths t _)              = getAttributes t

    type instance LabelType (WithBranchLengths t) = LabelType t
    getLabel  (WithBranchLengths t _) node = getLabel t node
    getLabels (WithBranchLengths t _) = getLabels t
    relabel newLabels (WithBranchLengths t lengths) = WithBranchLengths (relabel newLabels t) lengths

scaleBranchLengths factor g = modifyBranchLengths (\b -> factor * branchLength g b) g

instance IsGraph t => HasBranchLengths (WithBranchLengths t) where
    branchLength (WithBranchLengths tree ds) b = ds IntMap.! (undirectedName b)

instance IsGraph t => CanModifyBranchLengths (WithBranchLengths t) where
    modifyBranchLengths f t@(WithBranchLengths tree ds) = WithBranchLengths tree (IntMap.keysSet ds & IntMap.fromSet f)

branchLengthTree topology lengths = WithBranchLengths topology lengths

------------------ Labels ----------------

addLabels labels t = relabel newLabels t
    where newLabels = getNodesSet t & IntMap.fromSet (\node -> lookup node labels)

-- These two functions shouldn't go here -- but where should they go?
addInternalLabels tree = relabel newLabels tree where
    oldLabels = getLabels tree
    newLabels = getNodesSet tree & IntMap.fromSet newLabel

    newLabel node = case (oldLabels IntMap.! node) of
                      Just label -> Just label
                      Nothing -> Just $ T.append (T.singleton 'A') (T.pack (show node))

addAncestralLabel node labels = case (labels IntMap.! node) of
                                  Just l -> l
                                  Nothing -> T.append (T.singleton 'A') (T.pack (show node))


dropInternalLabels t = relabel newLabels t where
    labels = getLabels t
    newLabels = getNodesSet t & IntMap.fromSet (\node -> if nodeDegree t node <= 1 then labels IntMap.! node else Nothing)

---------------------------- Creating a graph from a list of edges ---------------------------
graphFromEdges nodes edges = Graph nodesMap branchesMap labels (noAttributesOn nodesSet) (noAttributesOn branchesSet) noAttributes where

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

    labels = nodesSet & IntMap.fromSet (\n -> Nothing)
