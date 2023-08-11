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

class Tree t where
    type family Unrooted t
    type family Rooted t

    findNode    :: t -> Int -> Node
    findEdge    :: t -> Int -> Edge
    getNodesSet :: t -> IntSet
    getEdgesSet :: t -> IntSet

    getNodeAttributes :: t -> Int -> Attributes
    getEdgeAttributes :: t -> Int -> Attributes
    getTreeAttributes :: t -> Attributes

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

edgesOutOfNodeSet tree nodeIndex = node_out_edges $ findNode tree nodeIndex
edgesOutOfNode tree nodeIndex = IntSet.toArray $ edgesOutOfNodeSet tree nodeIndex

class Tree t => HasBranchLengths t where
    branch_length :: t -> Int -> Double

class HasBranchLengths t => CanModifyBranchLengths t where
    modifyBranchLengths :: (Int -> Double) -> t -> t

class Tree t => HasRoot t where
    root :: t -> Int
    away_from_root :: t -> Int -> Bool

class HasRoot t => IsTimeTree t where
    node_time :: t -> Int -> Double

class IsTimeTree t => IsRateTimeTree t where
    branch_rate :: t -> Int -> Double

class Tree t => HasLabels t where
    get_label :: t -> Int -> Maybe Text
    -- TODO: all_labels - a sorted list of labels that serves as a kind of taxon-map?
    -- this would map integers to labels, and labels to integers, even if get_label
    -- indexes on nodes...
    -- TODO: make the C++ code handle this...
    
    get_labels :: t -> IntMap (Maybe Text)
    relabel :: IntMap (Maybe Text) -> t -> t

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

data RootedTreeImp t = RootedTree t Int (IntMap Bool)

data BranchLengthTreeImp t = BranchLengthTree t (IntMap Double)

data LabelledTreeImp t = LabelledTree t (IntMap (Maybe Text))

-- The array stores the node times
data TimeTreeImp t  = TimeTree t (IntMap Double)

-- The array stores the branch rates
data RateTimeTreeImp t = RateTimeTree t (IntMap Double)

instance Tree TreeImp where
    type instance Unrooted TreeImp = TreeImp
    type instance Rooted TreeImp = RootedTreeImp TreeImp

    getNodesSet (Tree nodesMap _  _ _ _)             = IntMap.keysSet nodesMap
    getEdgesSet (Tree _  edgesMap _ _ _)            = IntMap.keysSet edgesMap

    findNode    (Tree nodesMap _ _ _ _) node   = nodesMap IntMap.! node
    findEdge    (Tree _ edgesMap _ _ _) edge   = edgesMap IntMap.! edge

    getNodeAttributes (Tree _ _ a _ _) node         = a IntMap.! node
    getEdgeAttributes (Tree _ _ _ a _) edge         = a IntMap.! edge
    getTreeAttributes (Tree _ _ _ _ a)              = a

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

instance Tree t => Tree (RootedTreeImp t) where
    type Unrooted (RootedTreeImp t) = Unrooted t
    type Rooted   (RootedTreeImp t) = RootedTreeImp t

    getNodesSet (RootedTree t _ _)                 = getNodesSet t
    getEdgesSet (RootedTree t _ _)                 = getEdgesSet t
    findNode    (RootedTree t _ _) node            = findNode t node
    findEdge    (RootedTree t _ _) edgeIndex       = findEdge t edgeIndex
    getNodeAttributes (RootedTree t _ _) node         = getNodeAttributes t node
    getEdgeAttributes (RootedTree t _ _) edge         = getEdgeAttributes t edge
    getTreeAttributes (RootedTree t _ _)              = getTreeAttributes t

    unroot (RootedTree t _ _) = unroot t
    makeRooted t = t

instance Tree t => Tree (LabelledTreeImp t) where
    type Unrooted (LabelledTreeImp t) = LabelledTreeImp (Unrooted t)
    type Rooted (LabelledTreeImp t) = LabelledTreeImp (Rooted t)

    getNodesSet (LabelledTree t _)                 = getNodesSet t
    getEdgesSet (LabelledTree t _)                 = getEdgesSet t
    findNode    (LabelledTree t _) node            = findNode t node
    findEdge    (LabelledTree t _) edgeIndex       = findEdge t edgeIndex
    getNodeAttributes (LabelledTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (LabelledTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (LabelledTree t _)              = getTreeAttributes t

    unroot (LabelledTree t labels) = LabelledTree (unroot t) labels
    makeRooted (LabelledTree t labels) = LabelledTree (makeRooted t) labels


instance Tree t => Tree (BranchLengthTreeImp t) where
    type Unrooted (BranchLengthTreeImp t) = BranchLengthTreeImp (Unrooted t)
    type Rooted (BranchLengthTreeImp t) = BranchLengthTreeImp (Rooted t)

    getNodesSet (BranchLengthTree t _)             = getNodesSet t
    getEdgesSet (BranchLengthTree t _)             = getEdgesSet t
    findNode    (BranchLengthTree t _) node        = findNode t node
    findEdge    (BranchLengthTree t _) edgeIndex   = findEdge t edgeIndex
    getNodeAttributes (BranchLengthTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (BranchLengthTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (BranchLengthTree t _)              = getTreeAttributes t

    unroot (BranchLengthTree t lengths) = BranchLengthTree (unroot t) lengths
    makeRooted (BranchLengthTree t lengths) = BranchLengthTree (makeRooted t) lengths


instance HasRoot t => Tree (TimeTreeImp t) where
    type Unrooted (TimeTreeImp t) = BranchLengthTreeImp (Unrooted t)
    type Rooted   (TimeTreeImp t) = TimeTreeImp (Rooted t)

    getNodesSet (TimeTree t _)                     = getNodesSet t
    getEdgesSet (TimeTree t _)                     = getEdgesSet t
    findNode    (TimeTree t _) node                = findNode t node
    findEdge    (TimeTree t _) edgeIndex           = findEdge t edgeIndex
    getNodeAttributes (TimeTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (TimeTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (TimeTree t _)              = getTreeAttributes t

    unroot tt@(TimeTree t node_heights) = BranchLengthTree (unroot t) (getUEdgesSet tt & IntMap.fromSet (\b -> branch_length tt b))
    makeRooted (TimeTree t node_heights) = TimeTree (makeRooted t) node_heights


instance IsTimeTree t => Tree (RateTimeTreeImp t) where
    type Unrooted (RateTimeTreeImp t) = BranchLengthTreeImp (Unrooted t)
    type Rooted (RateTimeTreeImp t) = RateTimeTreeImp (Rooted t)

    getNodesSet (RateTimeTree t _)                 = getNodesSet t
    getEdgesSet (RateTimeTree t _)                 = getEdgesSet t
    findNode    (RateTimeTree t _) node            = findNode t node
    findEdge    (RateTimeTree t _) edgeIndex       = findEdge t edgeIndex
    getNodeAttributes (RateTimeTree t _) node         = getNodeAttributes t node
    getEdgeAttributes (RateTimeTree t _) edge         = getEdgeAttributes t edge
    getTreeAttributes (RateTimeTree t _)              = getTreeAttributes t

    unroot tt@(RateTimeTree t _) = BranchLengthTree (unroot t) (getUEdgesSet tt & IntMap.fromSet (\b -> branch_length tt b))
    makeRooted (RateTimeTree t branchRates) = RateTimeTree (makeRooted t) branchRates


instance HasRoot t => IsTimeTree (TimeTreeImp t) where
    node_time (TimeTree t hs) node = hs IntMap.! node

instance IsTimeTree t => IsTimeTree (LabelledTreeImp t) where
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

instance HasBranchLengths t => HasBranchLengths (LabelledTreeImp t) where
    branch_length (LabelledTree tree _) b = branch_length tree b

instance CanModifyBranchLengths t => CanModifyBranchLengths (LabelledTreeImp t) where
    modifyBranchLengths f (LabelledTree tree labels) = LabelledTree (modifyBranchLengths f tree) labels

scale_branch_lengths factor (BranchLengthTree t ds) = (BranchLengthTree t ds')
    where ds' = fmap (factor*) ds

-- Given that this is a tree, would numNodes t - numBranches t + 2 work for n_leaves >=3?
numLeaves t = length $ leaf_nodes t


instance Tree t => HasRoot (RootedTreeImp t) where
    root (RootedTree _ r _) = r
    away_from_root (RootedTree t r arr    ) b = arr IntMap.! b

instance HasRoot t => HasRoot (LabelledTreeImp t) where
    root (LabelledTree t _) = root t
    away_from_root (LabelledTree t _      ) b = away_from_root t b

instance HasRoot t => HasRoot (TimeTreeImp t) where
    root (TimeTree t _)     = root t
    away_from_root (TimeTree   t _        ) b = away_from_root t b

instance IsTimeTree t => HasRoot (RateTimeTreeImp t) where
    root (RateTimeTree t _) = root t
    away_from_root (RateTimeTree tree _  ) b = away_from_root tree b

instance HasRoot t => HasRoot (BranchLengthTreeImp t) where
    root (BranchLengthTree tree _) = root tree
    away_from_root (BranchLengthTree tree _  ) b = away_from_root tree b

-- Check for duplicate instances!

remove_root (RootedTree t _ _) = t
-- remove_root (LabelledTree t labels) = LabelledTree (remove_root t) labels

instance Tree t => HasLabels (LabelledTreeImp t) where
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
edgesTowardNode t node = fmap reverseEdge $ edgesOutOfNode t node
sourceNode  tree b = e_source_node  $ findEdge tree b
targetNode  tree b = e_target_node  $ findEdge tree b
edgeForNodes t (n1,n2) = fromJust $ find (\b -> targetNode t b == n2) (edgesOutOfNode t n1)
nodeDegree t n = IntSet.size (edgesOutOfNodeSet t n)
neighbors t n = fmap (targetNode t) (edgesOutOfNode t n)
edgesBeforeEdge t b = fmap reverseEdge $ IntSet.toArray $ IntSet.delete b (edgesOutOfNodeSet t node)
    where node = sourceNode t b
edgesAfterEdge t b = IntSet.toArray $ IntSet.delete (reverseEdge b) (edgesOutOfNodeSet t node)
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
           rt = RootedTree t r (getEdgesSet t & IntMap.fromSet check_away_from_root)

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
