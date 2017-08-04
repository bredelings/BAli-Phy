module Tree where
{
data Tree = Tree (Array [Int]) (Array (Int,Int,Int)) Int Int;  
  
numNodes (Tree _ _ n _) = n;
numBranches (Tree _ _ _ n) = n;
edgesOutOfNode (Tree nodesArray _ _ _) node = nodesArray ! node;
nodesForEdge (Tree _ branchesArray _ _) edgeIndex = branchesArray ! edgeIndex;
sourceNode  t b = case (nodesForEdge t b) of {(s,_,_,_)->s};
sourceIndex t b = case (nodesForEdge t b) of {(_,i,_,_)->i};
targetNode  t b = case (nodesForEdge t b) of {(_,_,t,_)->t};
reverseEdge t b = case (nodesForEdge t b) of {(_,_,_,r)->r};
edgeForNodes t (n1,n2) = head [b | b <- (edgesOutOfNode t n1), (targetNode t b)==n2];
nodeDegree t n = length (edgesOutOfNode t n);
neighbors t n = fmap (targetNode t) (edgesOutOfNode t n);
edgesBeforeEdge t b = map (reverseEdge t) $ filter (/=b) $ edgesOutOfNode t (sourceNode t b);
                                                                                                                     
is_leaf_node t n = (nodeDegree t n == 1);
is_internal_node t n = not $ is_leaf_node t n;

nodes t = [0..numNodes t - 1];
leaf_nodes t = filter (is_leaf_node t) (nodes t);
internal_nodes t = filter (is_internal_node t) (nodes t);
}
