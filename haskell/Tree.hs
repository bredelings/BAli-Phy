module Tree where

import Data.Foldable
import Data.Array
import Data.List (lookup)

import Data.Maybe (fromJust)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as Text

data Attributes = Attributes [(Text,Maybe Text)]

(Attributes cs1) +:+ (Attributes cs2) = Attributes (cs1 ++ cs2)

instance Show Attributes where
    show (Attributes []) = ""
    show (Attributes cs) = "[&" ++ intercalate "," (fmap go cs)  ++ "]" where
                       go (k, Nothing) = Text.unpack k
                       go (k, Just v)  = Text.unpack k ++ "=" ++ Text.unpack v

class Tree t where
    findNode    :: t -> Int -> Node
    findEdge    :: t -> Int -> Edge
    getNodesSet :: t -> IntSet
    getEdgesSet :: t -> IntSet

    getNodeAttributes :: t -> Int -> Attributes
    getEdgeAttributes :: t -> Int -> Attributes
    getTreeAttributes :: t -> Attributes

getNodes t = t & getNodesSet & IntSet.toList
numNodes t = t & getNodesSet & IntSet.size

getEdges t  = getEdgesSet t & IntSet.toList
getUEdges t = [ e | e <- getEdges t, e < (e_reverse $ findEdge t e)]
getUEdgesSet t = getUEdges t & IntSet.fromList
numBranches t = length $ getUEdges t

undirectedName t e = min e (reverseEdge t e)

edgesOutOfNode t node_index = findNode t node_index & node_out_edges

class Tree t => BranchLengthTree t where
    branch_length :: t -> Int -> Double

class Tree t => RootedTree t where
    root :: t -> Int
    away_from_root :: t -> Int -> Bool

class RootedTree t => TimeTree t where
    node_time :: t -> Int -> Double

class TimeTree t => RateTimeTree t where
    branch_rate :: t -> Int -> Double

class Tree t => LabelledTree t where
    get_label :: t -> Int -> Maybe Text
    -- TODO: all_labels - a sorted list of labels that serves as a kind of taxon-map?
    -- this would map integers to labels, and labels to integers, even if get_label
    -- indexes on nodes...
    -- TODO: make the C++ code handle this...
    
    get_labels :: t -> IntMap (Maybe Text)

-- OK, so should we store attributes inside the tree?
-- 

data Node = Node { node_name :: Int, node_out_edges:: Array Int Int}

instance Show Node where
    show (Node name out_edges) = "Node{node_name = " ++ show name ++ ", node_out_edges = " ++ show out_edges ++ "}"

-- ideally e_source_node and e_target_node would be of type Node,
--   and e_reverse would be of type Edge
data Edge = Edge { e_source_node, e_source_index, e_target_node, e_reverse, edge_name :: Int }

instance Show Edge where
    show (Edge source index target reverse name) = "Edge{e_source_node = " ++ show source ++ ", e_source_index = " ++ show index ++ ", e_target_node = " ++ show target ++ ", e_reverse = " ++ show reverse ++ ", edge_name = " ++ show name ++ "}"

data TreeImp = Tree (IntMap Node) (IntMap Edge) (IntMap Attributes) (IntMap Attributes) (Attributes)

data RootedTreeImp t = RootedTree t Int (IntMap Bool)

data BranchLengthTreeImp t = BranchLengthTree t (IntMap Double)

data LabelledTreeImp t = LabelledTree t (IntMap (Maybe Text))

-- The array stores the node times
data TimeTreeImp t  = TimeTree t (IntMap Double)

-- The array stores the branch rates
data RateTimeTreeImp t = RateTimeTree t (IntMap Double)

instance Tree TreeImp where
    getNodesSet (Tree nodesMap _  _ _ _)             = IntMap.keysSet nodesMap
    getEdgesSet (Tree _  edgesMap _ _ _)            = IntMap.keysSet edgesMap

    findNode    (Tree nodesMap _ _ _ _) node        = nodesMap IntMap.! node
    findEdge    (Tree _ edgesMap _ _ _) edgeIndex   = edgesMap IntMap.! edgeIndex

    getNodeAttributes (Tree _ _ a _ _) node         = a IntMap.! node
    getEdgeAttributes (Tree _ _ _ a _) edge         = a IntMap.! edge
    getTreeAttributes (Tree _ _ _ _ a)              = a

getNodeAttribute tree node key = lookup key ((\(Attributes as) -> as) $ getNodeAttributes tree node)
getEdgeAttribute tree edge key = lookup key ((\(Attributes as) -> as) $ getEdgeAttributes tree edge)
getTreeAttribute tree key = lookup key ((\(Attributes as) -> as) $ getTreeAttributes tree)
                                                      
instance Tree t => Tree (RootedTreeImp t) where
    getNodesSet (RootedTree t _ _)                 = getNodesSet t
    getEdgesSet (RootedTree t _ _)                 = getEdgesSet t
    findNode    (RootedTree t _ _) node            = findNode t node
    findEdge    (RootedTree t _ _) edgeIndex       = findEdge t edgeIndex
    getNodeAttributes (RootedTree t _ _) node         = getNodeAttributes t node
    getEdgeAttributes (RootedTree t _ _) edge         = getEdgeAttributes t edge
    getTreeAttributes (RootedTree t _ _)              = getTreeAttributes t

instance Tree t => Tree (LabelledTreeImp t) where
    getNodesSet (LabelledTree t _)                 = getNodesSet t
    getEdgesSet (LabelledTree t _)                 = getEdgesSet t
    findNode    (LabelledTree t _) node            = findNode t node
    findEdge    (LabelledTree t _) edgeIndex       = findEdge t edgeIndex
    getNodeAttributes (LabelledTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (LabelledTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (LabelledTree t _)              = getTreeAttributes t

instance Tree t => Tree (BranchLengthTreeImp t) where
    getNodesSet (BranchLengthTree t _)             = getNodesSet t
    getEdgesSet (BranchLengthTree t _)             = getEdgesSet t
    findNode    (BranchLengthTree t _) node        = findNode t node
    findEdge    (BranchLengthTree t _) edgeIndex   = findEdge t edgeIndex
    getNodeAttributes (BranchLengthTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (BranchLengthTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (BranchLengthTree t _)              = getTreeAttributes t

instance Tree t => Tree (TimeTreeImp t) where
    getNodesSet (TimeTree t _)                     = getNodesSet t
    getEdgesSet (TimeTree t _)                     = getEdgesSet t
    findNode    (TimeTree t _) node                = findNode t node
    findEdge    (TimeTree t _) edgeIndex           = findEdge t edgeIndex
    getNodeAttributes (TimeTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (TimeTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (TimeTree t _)              = getTreeAttributes t

instance Tree t => Tree (RateTimeTreeImp t) where
    getNodesSet (RateTimeTree t _)                 = getNodesSet t
    getEdgesSet (RateTimeTree t _)                 = getEdgesSet t
    findNode    (RateTimeTree t _) node            = findNode t node
    findEdge    (RateTimeTree t _) edgeIndex       = findEdge t edgeIndex
    getNodeAttributes (RateTimeTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (RateTimeTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (RateTimeTree t _)              = getTreeAttributes t

instance RootedTree t => TimeTree (TimeTreeImp t) where
    node_time (TimeTree t hs) node = hs IntMap.! node

instance TimeTree t => TimeTree (LabelledTreeImp t) where
    node_time (LabelledTree tt _) node = node_time tt node

instance TimeTree t => TimeTree (RateTimeTreeImp t) where
    node_time (RateTimeTree tt _) node = node_time tt node

instance TimeTree t => RateTimeTree (RateTimeTreeImp t) where
    branch_rate (RateTimeTree _ rs) node = rs IntMap.! node

branch_length_tree topology lengths = BranchLengthTree topology lengths' where
    lengths' = IntMap.fromList $ zip [0..] lengths

branch_lengths (BranchLengthTree _ ds) = ds

time_tree topology times = TimeTree topology times

rate_time_tree time_tree rates = RateTimeTree time_tree rates

branch_duration t b = abs (node_time t source - node_time t target)
    where source = sourceNode t b
          target = targetNode t b

instance Tree t => BranchLengthTree (BranchLengthTreeImp t) where
    branch_length (BranchLengthTree tree ds) b = ds IntMap.! b' where b' = min b (reverseEdge tree b)

instance TimeTree t => BranchLengthTree (RateTimeTreeImp t) where
    branch_length tree b = branch_duration tree b * branch_rate tree b

instance RootedTree t => BranchLengthTree (TimeTreeImp t) where
    branch_length tree b = branch_duration tree b

instance BranchLengthTree t => BranchLengthTree (LabelledTreeImp t) where
    branch_length (LabelledTree tree _) b = branch_length tree b

scale_branch_lengths factor (BranchLengthTree t ds) = (BranchLengthTree t ds')
    where ds' = fmap (factor*) ds

-- Given that this is a tree, would numNodes t - numBranches t + 2 work for n_leaves >=3?
numLeaves t = length $ leaf_nodes t


instance Tree t => RootedTree (RootedTreeImp t) where
    root (RootedTree _ r _) = r
    away_from_root (RootedTree t r arr    ) b = arr IntMap.! b

instance RootedTree t => RootedTree (LabelledTreeImp t) where
    root (LabelledTree t _) = root t
    away_from_root (LabelledTree t _      ) b = away_from_root t b

instance RootedTree t => RootedTree (TimeTreeImp t) where
    root (TimeTree t _)     = root t
    away_from_root (TimeTree   t _        ) b = away_from_root t b

instance RootedTree t => RootedTree (RateTimeTreeImp t) where
    root (RateTimeTree t _) = root t
    away_from_root (RateTimeTree tree _  ) b = away_from_root tree b

instance RootedTree t => RootedTree (BranchLengthTreeImp t) where
    root (BranchLengthTree tree _) = root tree
    away_from_root (BranchLengthTree tree _  ) b = away_from_root tree b

-- Check for duplicate instances!

remove_root (RootedTree t _ _) = t
-- remove_root (LabelledTree t labels) = LabelledTree (remove_root t) labels

instance Tree t => LabelledTree (LabelledTreeImp t) where
    get_label  (LabelledTree _ labels) node = labels IntMap.! node
    get_labels (LabelledTree _ labels) = labels

instance LabelledTree t => LabelledTree (BranchLengthTreeImp t) where
    get_label  (BranchLengthTree t _) node = get_label t node
    get_labels (BranchLengthTree t _) = get_labels t

instance LabelledTree t => LabelledTree (TimeTreeImp t) where
    get_label (TimeTree t _) node          = get_label t node
    get_labels (TimeTree t _) = get_labels t

instance LabelledTree t => LabelledTree (RateTimeTreeImp t) where
    get_label (RateTimeTree t _) node      = get_label t node
    get_labels (RateTimeTree t _) = get_labels t


toward_root rt b = not $ away_from_root rt b

parentBranch rooted_tree n = find (toward_root rooted_tree) (edgesOutOfNode rooted_tree n)

parentNode rooted_tree n = case parentBranch rooted_tree n of Just b  -> Just $ targetNode rooted_tree b
                                                              Nothing -> Nothing

-- For numNodes, numBranches, edgesOutOfNode, and findEdge I'm currently using fake polymorphism
edgesTowardNode t node = fmap (reverseEdge t) $ edgesOutOfNode t node
sourceNode  tree b = e_source_node  $ findEdge tree b
sourceIndex tree b = e_source_index $ findEdge tree b
targetNode  tree b = e_target_node  $ findEdge tree b
reverseEdge tree b = e_reverse      $ findEdge tree b
edgeForNodes t (n1,n2) = fromJust $ find (\b -> targetNode t b == n2) (edgesOutOfNode t n1)
nodeDegree t n = length (edgesOutOfNode t n)
neighbors t n = fmap (targetNode t) (edgesOutOfNode t n)
edgesBeforeEdge t b = let e = findEdge t b
                          source = e_source_node $ e
                          index = e_source_index $ e
                      in fmap (reverseEdge t) $ removeElement index $ edgesOutOfNode t source
edgesAfterEdge t b  = fmap (reverseEdge t) $ edgesBeforeEdge t $ reverseEdge t b

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

tree_from_edges nodes edges = Tree nodesMap branchesMap IntMap.empty IntMap.empty (Attributes []) where

    num_nodes = length nodes

    -- FIX: this is a way to avoid depending changeables when |edges| is constant, but edges is changeable.
    num_branches = num_nodes - 1

    -- is this really how we want to name the branches?
    forward_backward_edges = zip [0..] $ edges ++ map swap edges

    reverse b = (b + num_branches) `mod` (2*num_branches)

    find_branch b = fmap snd $ find (\(b',_) -> b' == b) forward_backward_edges

    nodesSet = IntSet.fromList nodes
    nodesMap = IntMap.fromSet (\n ->  Node n (listArray' [b | (b,(x,y)) <- forward_backward_edges, x==n]) ) nodesSet

    branchesSet = IntSet.fromList [0..2*num_branches-1]
    branchesMap = IntMap.fromSet (\b -> let Just (s,t) = find_branch b
                                            Just i     = elemIndexArray b (node_out_edges (nodesMap IntMap.! s))
                                        in Edge s i t (reverse b) b) branchesSet

tree_length tree = sum [ branch_length tree b | b <- getUEdges tree ]

allEdgesAfterEdge tree b = b:concatMap (allEdgesAfterEdge tree) (edgesAfterEdge tree b)
allEdgesFromNode tree n = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree n)

add_labels labels t = LabelledTree t (getNodesSet t & IntMap.fromSet (\node -> lookup node labels))

add_root r t = rt
     where check_away_from_root b = (sourceNode rt b == root rt) || (or $ fmap (away_from_root rt) (edgesBeforeEdge rt b))
           nb = numBranches t * 2
           rt = RootedTree t r (getEdgesSet t & IntMap.fromSet check_away_from_root)

make_rooted tree = add_root root tree
    where root = head $ (internal_nodes tree ++ leaf_nodes tree)

-- These two functions shouldn't go here -- but where should they go?
addInternalLabels tree = LabelledTree tree newLabels where
    oldLabels = get_labels tree
    newLabels = getNodesSet tree & IntMap.fromSet newLabel

    newLabel node = case (oldLabels IntMap.! node) of
                      Just label -> Just label
                      Nothing -> Just $ Text.append (Text.singleton 'A') (Text.pack (show node))

add_ancestral_label node labels = case (labels IntMap.! node) of
                                    Just l -> l
                                    Nothing -> Text.append (Text.singleton 'A') (Text.pack (show node))

