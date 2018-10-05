module Tree where

data Tree = Tree (Array Int [Int]) (Array Int (Int,Int,Int)) Int Int
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
sourceNode  t b = case (nodesForEdge t b) of (s,_,_,_)->s
sourceIndex t b = case (nodesForEdge t b) of (_,i,_,_)->i
targetNode  t b = case (nodesForEdge t b) of (_,_,t,_)->t
reverseEdge t b = case (nodesForEdge t b) of (_,_,_,r)->r
edgeForNodes t (n1,n2) = head [b | b <- (edgesOutOfNode t n1), (targetNode t b)==n2]
nodeDegree t n = length (edgesOutOfNode t n)
neighbors t n = fmap (targetNode t) (edgesOutOfNode t n)
edgesBeforeEdge t b = case (nodesForEdge t b) of (source,index,_,_) -> map (reverseEdge t) $ remove_element index $ edgesOutOfNode t source

is_leaf_node t n = (nodeDegree t n == 1)
is_internal_node t n = not $ is_leaf_node t n

nodes t = [0..numNodes t - 1]
leaf_nodes t = filter (is_leaf_node t) (nodes t)
internal_nodes t = filter (is_internal_node t) (nodes t)

remove_element _ []     = []
remove_element 0 (x:xs) = xs
remove_element i (x:xs) = x:(remove_element (i-1) xs)

