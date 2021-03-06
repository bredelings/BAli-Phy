module Tree where

data Tree = Tree (Array Int [Int]) (Array Int (Int,Int,Int,Int)) Int
-- Polymorphism here really should be handled with a class that has the members below
-- If we allow adding branches to functions later, we could move polymorphic definitions into files. e.g. for show.
data RootedTree = RootedTree Tree Int (Array Int Bool)

data LabelledTree = LabelledTree Tree [String]

-- Here the Tree/RootedTree are _classes_ not objects...
-- Should we make LabelledTree,LabelledBranchLengthTree,LabelledNodeHeighTree
--  and have them implement get_labels?
-- It looks like add_labels would be a bit more complicated...
data BranchLengthTree = BranchLengthTree Tree (Array Double)

-- The array stores the node times
data TimeTree   = TimeTree RootedTree (Array Double)

-- The array stores the branch rates
data RateTimeTree = RateTimeTree TimeTree (Array Double)

edgesOutOfNode (Tree nodesArray _ _) node = nodesArray ! node
edgesOutOfNode (RootedTree t _ _) node      = edgesOutOfNode t node
edgesOutOfNode (LabelledTree t _) node      = edgesOutOfNode t node
edgesOutOfNode (BranchLengthTree t _) node  = edgesOutOfNode t node
edgesOutOfNode (TimeTree t _) node          = edgesOutOfNode t node
edgesOutOfNode (RateTimeTree t _) node      = edgesOutOfNode t node

nodesForEdge (Tree _ branchesArray _) edgeIndex   = branchesArray ! edgeIndex
nodesForEdge (RootedTree t _ _) edgeIndex         = nodesForEdge t edgeIndex
nodesForEdge (LabelledTree t _) edgeIndex         = nodesForEdge t edgeIndex
nodesForEdge (BranchLengthTree t _) edgeIndex     = nodesForEdge t edgeIndex
nodesForEdge (TimeTree t _) edgeIndex             = nodesForEdge t edgeIndex
nodesForEdge (RateTimeTree t _) edgeIndex         = nodesForEdge t edgeIndex

numNodes (Tree _ _ n)           = n
numNodes (RootedTree t _ _)     = numNodes t
numNodes (LabelledTree t _)     = numNodes t
numNodes (BranchLengthTree t _) = numNodes t
numNodes (TimeTree t _)         = numNodes t
numNodes (RateTimeTree t _)     = numNodes t

numBranches t = numNodes t - 1

branch_length_tree topology lengths = BranchLengthTree topology (listArray' lengths)

branch_lengths   (BranchLengthTree _ ds) = ds

time_tree topology times = TimeTree topology (listArray n times) where n = numNodes topology
node_times (TimeTree t hs) = hs
node_times (RateTimeTree tt _) = node_times tt
node_time t n = (node_times t)!n

rate_time_tree time_tree rates = RateTimeTree time_tree (listArray nb rates) where nb = numBranches time_tree
branch_rates (RateTimeTree _ rs) = rs
branch_rate t b = (branch_rates t)!b

branch_duration t b = abs (node_time t source - node_time t target)
    where source = sourceNode t b
          target = targetNode t b

branch_length (BranchLengthTree _ ds) b = ds!b
branch_length t@(TimeTree _  _) b = branch_duration t b
branch_length t@(RateTimeTree _  _) b = branch_duration t b * branch_rate t b

scale_branch_lengths factor (BranchLengthTree t ds) = (BranchLengthTree t ds')
    where ds' = arrayMap (factor*) ds

-- Given that this is a tree, would numNodes t - numBranches t + 2 work for n_leaves >=3?
numLeaves t = length $ leaf_nodes t

root (RootedTree _ r _) = r
root (LabelledTree t _) = root t
root (TimeTree t _)     = root t
root (RateTimeTree t _) = root t

remove_root (RootedTree t _ _) = t
remove_root (LabelledTree t labels) = LabelledTree (remove_root t) labels

get_labels (Tree _ _ _)            = error "get_labels: trying to get labels from an unlabelled tree!"
get_labels (RootedTree _ _ _)      = error "get_labels: trying to get labels from an unlabelled tree!"
get_labels (LabelledTree _ labels) = labels
get_labels (BranchLengthTree t _)  = get_labels t
get_labels (TimeTree t _)          = get_labels t
get_labels (RateTimeTree t _)      = get_labels t

add_labels labels t@(Tree _ _ _)          = LabelledTree t labels
add_labels labels rt@(RootedTree _ _ _)   = LabelledTree rt labels
add_labels labels (LabelledTree _ _)      = error "add_labels: trying to add labels to an already-labelled tree!"
add_labels labels (BranchLengthTree t ds) = BranchLengthTree (add_labels labels t) ds
add_labels labels (TimeTree t hs)         = TimeTree (add_labels labels t) hs
add_labels labels (RateTimeTree t hs)     = RateTimeTree (add_labels labels t) hs

add_root r (LabelledTree t labels) = LabelledTree (add_root r t) labels
add_root r (BranchLengthTree t ds) = BranchLengthTree (add_root r t) ds
add_root r t = rt
    where check_away_from_root b = (sourceNode rt b == root rt) || (or $ map (away_from_root rt) (edgesBeforeEdge rt b))
          nb = numBranches t * 2
          rt = RootedTree t r (mkArray nb check_away_from_root)

make_rooted tree = add_root (numNodes tree - 1) tree

away_from_root (RootedTree t r arr    ) b = arr!b
away_from_root (LabelledTree t _      ) b = away_from_root t b
away_from_root (BranchLengthTree t _  ) b = away_from_root t b
away_from_root (TimeTree   t _        ) b = away_from_root t b
away_from_root (RateTimeTree t _      ) b = away_from_root t b
away_from_root (Tree _ _ _            ) b = error "away_from_root: unrooted tree!"

toward_root    rt b = not $ away_from_root rt b

parentBranch rooted_tree n = listToMaybe [b | b <- edgesOutOfNode rooted_tree n, toward_root rooted_tree b]

parentNode rooted_tree n = case parentBranch rooted_tree n of Just b  -> Just $ targetNode rooted_tree b
                                                              Nothing -> Nothing

-- For numNodes, numBranches, edgesOutOfNode, and nodesForEdge I'm currently using fake polymorphism
edgesTowardNode t node = map (reverseEdge t) $ edgesOutOfNode t node
sourceNode  tree b = let (s,_,_,_) = nodesForEdge tree b in s
sourceIndex tree b = let (_,i,_,_) = nodesForEdge tree b in i
targetNode  tree b = let (_,_,t,_) = nodesForEdge tree b in t
reverseEdge tree b = let (_,_,_,r) = nodesForEdge tree b in r
edgeForNodes t (n1,n2) = head [b | b <- (edgesOutOfNode t n1), (targetNode t b)==n2]
nodeDegree t n = length (edgesOutOfNode t n)
neighbors t n = map (targetNode t) (edgesOutOfNode t n)
edgesBeforeEdge t b = let (source,index,_,_) = nodesForEdge t b
                      in map (reverseEdge t) $ remove_element index $ edgesOutOfNode t source
edgesAfterEdge t b  = map (reverseEdge t) $ edgesBeforeEdge t $ reverseEdge t b

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

tree_from_edges num_nodes edges = Tree nodesArray (listArray (2*num_branches) branches) num_nodes where

    num_branches   = num_nodes - 1

    branch_edges   = forward_edges++backward_edges where
        forward_edges  = zip [0..] edges
        backward_edges = zip [num_branches..] (map swap edges)

    reverse b = (b + num_branches) `mod` (2*num_branches)

    find_branch b = listToMaybe [(s,t) | (b',(s,t)) <- branch_edges, b==b']

    nodesArray = listArray num_nodes nodes where
        nodes = [ [b | (b,(x,y)) <- branch_edges, x==n] | n <- [0..num_nodes-1]]

    branches = [ let Just (s,t) = find_branch b
                     Just i     = elemIndex b (nodesArray!s)
                 in (s,i,t,reverse b) | b <- [0..2*num_branches-1] ]


