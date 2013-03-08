module Tree where
{
data Tree = Tree (Array [Int]) (Array [Int]) Int Int;  
  
numNodes (Tree _ _ n _) = n;
numBranches (Tree _ _ _ n) = n;
edgesOutOfNode (Tree nodesArray _ _ _) node = nodesArray ! node;
nodesForEdge (Tree _ branchesArray _ _) edgeIndex = branchesArray ! edgeIndex;
sourceNode t edge = fst (nodesForEdge t edge);
targetNode t edge = snd (nodesForEdge t edge);
edgeForNodes t (n1,n2) = head [b | b <- (edgesOutOfNode t n1), (targetNode t b)==n2];
reverseEdge t b = edgeForNodes (swap (nodesForEdge t b));
nodeDegree t n = length (edgesOutOfNode t n);
neighbors t n = fmap (targetNode t) (edgesOutOfNode t n);
edgesBeforeEdge t b = case (nodesForEdge t b) of {(n1,n2) -> [edgeForNodes t (n,n1) | n <- neighbors t n1, n /= n2 ]};

is_leaf_node t n = (nodeDegree t n == 1);
is_internal_node t n = not $ is_leaf_node t n;

nodes t = [0..numNodes t - 1];
leaf_nodes t = filter is_leaf_node (nodes t);
internal_nodes t = filter is_internal_node (nodes t);
}