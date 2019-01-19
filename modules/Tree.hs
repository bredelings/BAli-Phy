module Tree where

data Tree = Tree (Array Int [Int]) (Array Int (Int,Int,Int,Int)) Int Int
-- Polymorphism here really should be handled with a class that has the members below
-- If we allow adding branches to functions later, we could move polymorphic definitions into files. e.g. for show.
data RootedTree = RootedTree Tree Int (Array Int Bool)

root (RootedTree _ r _) = r
rooted_tree t r = RootedTree t r (mkArray n check_away_from_root)
    where check_away_from_root b = sourceNode t b == root t || or $ map (away_from_root t) (edgesBeforeEdge t)
          n = numBranches t * 2

away_from_root (RootedTree t r arr) b = arr!b

-- For numNodes, numBranches, edgesOutOfNode, and nodesForEdge I'm currently using fake polymorphism
numNodes (Tree _ _ n _) = n
numNodes (RootedTree t _ _) = numNodes t
numBranches (Tree _ _ _ n) = n
numBranches (RootedTree t _ _) = numBranches t
edgesOutOfNode (Tree nodesArray _ _ _) node = nodesArray ! node
edgesOutOfNode (RootedTree t _ _) node = edgesOutOfNode t node
nodesForEdge (Tree _ branchesArray _ _) edgeIndex = branchesArray ! edgeIndex
nodesForEdge (RootedTree t _ _) edgeIndex = nodesForEdge t edgeIndex
sourceNode  tree b = let (s,_,_,_) = nodesForEdge tree b in s
sourceIndex tree b = let (_,i,_,_) = nodesForEdge tree b in i
targetNode  tree b = let (_,_,t,_) = nodesForEdge tree b in t
reverseEdge tree b = let (_,_,_,r) = nodesForEdge tree b in r
edgeForNodes t (n1,n2) = head [b | b <- (edgesOutOfNode t n1), (targetNode t b)==n2]
nodeDegree t n = length (edgesOutOfNode t n)
neighbors t n = fmap (targetNode t) (edgesOutOfNode t n)
edgesBeforeEdge t b = let (source,index,_,_) = nodesForEdge t b
                      in map (reverseEdge t) $ remove_element index $ edgesOutOfNode t source
edgesAfterEdge t b  = map (reverseEdge t) $ edgesBeforeEdge t $ reverseEdge t b

is_leaf_node t n = (nodeDegree t n == 1)
is_internal_node t n = not $ is_leaf_node t n

nodes t = [0..numNodes t - 1]
leaf_nodes t = filter (is_leaf_node t) (nodes t)
internal_nodes t = filter (is_internal_node t) (nodes t)

remove_element _ []     = []
remove_element 0 (x:xs) = xs
remove_element i (x:xs) = x:(remove_element (i-1) xs)

write_newick t@(Tree _ _ _ _) = write_newick_node 0 t
write_newick rt@(RootedTree t r _) = write_newick_node r t

write_newick_node root tree = (write_branches_and_node tree (edgesOutOfNode tree root) root) ++ ";" where
    write_branches_and_node tree branches node = write_branches tree branches ++ show node

    write_branches tree [] = ""
    write_branches tree branches = "(" ++ text ++ ")" where
        text = intercalate "," $ map (write_branch tree) $ branches

    write_branch tree branch = write_branches_and_node tree (edgesAfterEdge tree branch) (targetNode tree branch)

tree_from_edges num_nodes edges = Tree nodesArray (listArray' branches) num_nodes num_branches where

    num_branches   = length edges

    branch_edges   = forward_edges++backward_edges where
        forward_edges  = zip [0..] edges
        backward_edges = zip [num_branches..] (map swap edges)

    reverse b = (b + num_branches) `mod` (2*num_branches)

    find_branch b = listToMaybe [(s,t) | (b',(s,t)) <- branch_edges, b==b']

    nodesArray = listArray' nodes where
        nodes = [ [b | (b,(x,y)) <- branch_edges, x==n] | n <- [0..num_nodes-1]]

    branches = [ let Just (s,t) = find_branch b
                     Just i     = elemIndex b (nodesArray!s)
                 in (s,i,t,reverse b) | b <- [0..2*num_branches-1] ]
