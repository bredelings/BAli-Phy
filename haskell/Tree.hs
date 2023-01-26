module Tree where

import Data.Foldable
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

class Tree t where
    findNode :: t -> Int -> Node
    findEdge :: t -> Int -> Edge
    getNodes :: t -> IntSet

numNodes t = t & getNodes & IntSet.size

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
    get_label :: t -> Int -> Maybe String
    get_labels :: t -> Array Int String

data Node = Node { node_name :: Int, node_out_edges:: Array Int Int}

data Edge = Edge { e_source_node, e_source_index, e_target_node, e_reverse, edge_name :: Int }

data TreeImp = Tree (IntMap Node) (Array Int Edge)

data RootedTreeImp t = RootedTree t Int (Array Int Bool)

data BranchLengthTreeImp t = BranchLengthTree t (Array Int Double)

data LabelledTreeImp t = LabelledTree t (Array Int String)

-- The array stores the node times
data TimeTreeImp t  = TimeTree t (IntMap Double)

-- The array stores the branch rates
data RateTimeTreeImp t = RateTimeTree t (Array Int Double)

instance Tree TreeImp where
    getNodes (Tree nodesMap _)                = IntMap.keysSet nodesMap
    findNode (Tree nodesMap _) node           = nodesMap IntMap.! node
    findEdge (Tree _ branchesArray) edgeIndex = branchesArray ! edgeIndex

instance Tree t => Tree (RootedTreeImp t) where
    getNodes (RootedTree t _ _)                 = getNodes t
    findNode (RootedTree t _ _) node            = findNode t node
    findEdge (RootedTree t _ _) edgeIndex       = findEdge t edgeIndex

instance Tree t => Tree (LabelledTreeImp t) where
    getNodes (LabelledTree t _)                 = getNodes t
    findNode (LabelledTree t _) node            = findNode t node
    findEdge (LabelledTree t _) edgeIndex       = findEdge t edgeIndex

instance Tree t => Tree (BranchLengthTreeImp t) where
    getNodes (BranchLengthTree t _)             = getNodes t
    findNode (BranchLengthTree t _) node        = findNode t node
    findEdge (BranchLengthTree t _) edgeIndex   = findEdge t edgeIndex

instance Tree t => Tree (TimeTreeImp t) where
    getNodes (TimeTree t _)                     = getNodes t
    findNode (TimeTree t _) node                = findNode t node
    findEdge (TimeTree t _) edgeIndex           = findEdge t edgeIndex

instance Tree t => Tree (RateTimeTreeImp t) where
    getNodes (RateTimeTree t _)                 = getNodes t
    findNode (RateTimeTree t _) node            = findNode t node
    findEdge (RateTimeTree t _) edgeIndex       = findEdge t edgeIndex

instance RootedTree t => TimeTree (TimeTreeImp t) where
    node_time (TimeTree t hs) node = hs IntMap.! node

instance TimeTree t => TimeTree (LabelledTreeImp t) where
    node_time (LabelledTree tt _) node = node_time tt node

instance TimeTree t => TimeTree (RateTimeTreeImp t) where
    node_time (RateTimeTree tt _) node = node_time tt node

instance TimeTree t => RateTimeTree (RateTimeTreeImp t) where
    branch_rate (RateTimeTree _ rs) node = rs!node

numBranches t = numNodes t - 1

branch_length_tree topology lengths = BranchLengthTree topology (listArray' lengths)

branch_lengths (BranchLengthTree _ ds) = ds

time_tree topology times = TimeTree topology times

rate_time_tree time_tree rates = RateTimeTree time_tree (listArray nb rates) where nb = numBranches time_tree

branch_duration t b = abs (node_time t source - node_time t target)
    where source = sourceNode t b
          target = targetNode t b

instance Tree t => BranchLengthTree (BranchLengthTreeImp t) where
    branch_length (BranchLengthTree tree ds) b = ds!b' where b' = min b (reverseEdge tree b)

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
    away_from_root (RootedTree t r arr    ) b = arr!b

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
    get_label  (LabelledTree _ labels) node = if inRange (bounds labels) node then Just (labels!node) else Nothing
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

nodes t = [0..numNodes t - 1]
leaf_nodes t = filter (is_leaf_node t) (nodes t)
internal_nodes t = filter (is_internal_node t) (nodes t)

remove_element _ []     = []
remove_element 0 (x:xs) = xs
remove_element i (x:xs) = x:(remove_element (i-1) xs)

tree_from_edges num_nodes edges = Tree nodesMap (listArray (2*num_branches) branches) where

    num_branches   = num_nodes - 1

    branch_edges   = forward_edges++backward_edges where
        forward_edges  = zip [0..] edges
        backward_edges = zip [num_branches..] (map swap edges)

    reverse b = (b + num_branches) `mod` (2*num_branches)

    find_branch b = fmap snd $ find (\(b',_) -> b' == b) branch_edges

    nodesSet = IntSet.fromList [0..num_nodes-1]
    nodesMap = IntMap.fromSet (\n ->  Node n (listArray' [b | (b,(x,y)) <- branch_edges, x==n]) ) nodesSet

    branches = [ let Just (s,t) = find_branch b
                     Just i     = elemIndexArray b (node_out_edges (nodesMap IntMap.! s))
                 in Edge s i t (reverse b) b | b <- [0..2*num_branches-1] ]

tree_length tree = sum [ branch_length tree b | b <- [0..numBranches tree - 1]]

allEdgesAfterEdge tree b = b:concatMap (allEdgesAfterEdge tree) (edgesAfterEdge tree b)
allEdgesFromNode tree n = concatMap (allEdgesAfterEdge tree) (edgesOutOfNode tree n)

add_labels labels t = LabelledTree t (listArray' labels)

add_root r t = rt
     where check_away_from_root b = (sourceNode rt b == root rt) || (or $ fmap (away_from_root rt) (edgesBeforeEdge rt b))
           nb = numBranches t * 2
           rt = RootedTree t r (mkArray nb check_away_from_root)

make_rooted tree = add_root (numNodes tree - 1) tree

