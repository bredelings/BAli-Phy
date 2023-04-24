{-# LANGUAGE RecursiveDo #-}
module BirthDeath where
-- See https://hackage.haskell.org/package/elynx-tree-0.3.2
-- See also Probability/Distribution/Tree/BirthDeath.hs

import Probability
import Data.Array

type Time = Double

-- If we're going to make the tree traversable or foldable, then we'd need to
-- store some data on each node.  Should we consider the Time to be such data?
-- Maybe, but we could, in theory annotate the tree with additional data.

data Event a = Birth (Next a) (Next a) |
               Sample (Next a) |
               Death |
               Finish

-- We could also include events Start1 and Start2.

data Next a = Next a (Event a) (Either (Tree a) (Next a))

data Tree a = Start1 a (Next a) | Start2 a (Next a) (Next a)

{- * If we store the branch duration, then we could shift a subtree by shrinking one branch.
       We could shrink one branch and lengthen child branches to move just one node.
       But we would still probably have to recalculate the node times after.

   * If we store the end time, then we could move a single node without having to adjust other nodes.
       But if we want to shift an entire subtree, then we have to visit all the times and update them.
-}

bd1' lambda mu t1 t2 prev = do
  -- Get the time of the next event
  t <- (t1 +) <$> (sample (exponential (1/(lambda+mu))))

  -- Determine if its a birth or death
  death <- sample (bernoulli (mu/(lambda+mu)))

  if t > t2 then
     return $ Next t2 Finish prev
  else if death == 1 then
     return $ Next t Death prev
  else
      do rec tree1 <- bd1' lambda mu t t2 (Right node)
             tree2 <- bd1' lambda mu t t2 (Right node)
             let node = Next t (Birth tree1 tree2) prev
         return node


bd1 lambda mu t1 t2 = do
  rec next <- bd1' lambda mu t1 t2 (Left tree)
      let tree = Start1 t1 next
  return tree

bd2 lambda mu t1 t2 = do
  rec next1 <- bd1' lambda mu t1 t2 (Left tree)
      next2 <- bd1' lambda mu t1 t2 (Left tree)
      let tree = Start2 t1 next1 next2
  return tree


type Node a = Either (Tree a) (Next a)

-- If Direction is ToRoot, we have (node, parent_node)
-- If Direction is FromROot, we have (parent_node, node)
data Direction = ToRoot | FromRoot

data Edge a = Edge (Next a) Direction

--- Hmm... in both of these cases, we walk the tree and sum an integer for each node.

fmapish f prev (Next y event _) = let p = Next (f y) (go p event) prev in p
    where go p (Birth n1 n2) = Birth (fmapish f (Right p) n1) (fmapish f (Right p) n2)
          go p (Sample n1)   = Sample (fmapish f (Right p) n1)
          go p Death         = Death
          go p Finish        = Finish

instance Functor Tree where
    fmap f tree@(Start1 x n1)    = let start = Start1 (f x) (fmapish f (Left start) n1) in start
    fmap f tree@(Start2 x n1 n2) = let start = Start2 (f x) (fmapish f (Left start) n1) (fmapish f (Left start) n2) in start


toListish (Next x event _) = x:go event where
    go (Birth n1 n2) = toListish n1 ++ toListish n2
    go (Sample n1) = toListish n1
    go Death = []
    go Finish = []

instance Foldable Tree where
    toList (Start1 x n1) = x:toListish n1
    toList (Start2 x n1 n2) = x:(toListish n1++toListish n2)


-- PROBLEM: we need to access the whole node in order to count the number of leaves, branches, etc.

node_out_edges :: Node a -> Array Int (Edge a)
node_out_edges (Left (Start1 _ node)) = listArray' [Edge node FromRoot]
node_out_edges (Left (Start2 _ node1 node2)) = listArray' [Edge node1 FromRoot, Edge node2 FromRoot]
node_out_edges (Right n1@(Next _ (Birth n2 n3) _)) = listArray' [Edge n1 ToRoot, Edge n2 FromRoot, Edge n3 FromRoot]
node_out_edges (Right n1@(Next _ (Sample n2) _)) = listArray' [Edge n1 ToRoot, Edge n2 FromRoot]
node_out_edges (Right n@(Next _ Finish _)) = listArray' [Edge n ToRoot]
node_out_edges (Right n@(Next _ Death _)) = listArray' [Edge n ToRoot]

sourceNode :: Edge a -> Node a
sourceNode (Edge node ToRoot) = Right node
sourceNode (Edge (Next _ _ parent) FromRoot) = parent

targetNode :: Edge a -> Node a
targetNode (Edge node FromRoot) = Right node
targetNode (Edge (Next _ _ parent) ToRoot) = parent

reverse_edge :: Edge a -> Edge a
reverse_edge (Edge node FromRoot) = Edge node ToRoot
reverse_edge (Edge node ToRoot) = Edge node FromRoot

is_leaf_node (Right (Next _ Finish _)) = True
is_leaf_node (Right (Next _ Death  _)) = True
is_leaf_node _                         = False

is_internal_node n = not $ is_leaf_node n

is_internal_branch b = is_internal_node (sourceNode b) && is_internal_node (targetNode b)

is_leaf_branch b = not $ is_internal_branch b

away_from_root (Edge _ FromRoot) = True
away_from_root _                 = False

toward_root = not . away_from_root

parentBranch (Left _) = Nothing
parentBranch (Right node) = Just $ Edge node ToRoot

parentNode node = fmap targetNode $ parentBranch node


{-


-- findNode :: t -> Node -> Array Int Edge
   --  where Edge knows its source, target, reverse_edge, and label
   --
   -- getNodesSet :: t -> IntSet
   nodes :: tree -> node
   edgesTowardNode :: tree -> node -> [edge]

   sourceNode :: edge -> node
   targetNode :: edge -> node
   sourceIndex :: edge -> Int
   reverse_edge :: edge -> edge
   edge_name :: e -> Int

   reverseEdge :: Edge t ->  Edge t
   edgeForNodes :: t -> (Node t, Node t) -> Edge t
   nodeDegree :: t -> Node t -> Int
   neighbors :: t -> Node -> [Node]
   edgesBeforeEdge :: t -> Edge t -> [Edge t]
   edgesAfterEdge :: t -> Edge t -> [Edge t]
   is_leaf_node :: t -> Node t -> Bool
   is_internal_node :: t -> Node t -> bool
   is_internal_branch 
-}

-- OK, so for a tree, we need to get unique names for each node.
-- The problem is that we want to decouple name generation for
-- left and right subtrees of a Birth node, but its not clear how.
-- * If we used pointer addresses, that would work, but we don't have that.
-- * We could generate random integers, and then afterward go fix up.
-- * We could use IO to get unique names, sharing a unique-name source.
-- * We could pass a unique integer generator to the left subtree first,
--   and then to the right subtree.

-- instance Tree Event where
{-
    findNode    :: t -> Int -> Node
    findEdge    :: t -> Int -> Edge
    getNodesSet :: t -> IntSet
    getEdgesSet :: t -> IntSet
-}

{-
Can we generalize to avoid requiring integer names?
For the birth-death trees here, it seems like we could just reference the nodes directly.   
Currently, Tree.hs uses equality for a few things:
  * in edgeForNodes t (n1,n2), we walk all branches out of n1 and check if the target is n2
  * in tree_from_edges, we walk the list of (b',(n1,n2)) to find the entry with b' == b
  * in add_root, we need to check if a node is the root.

But we don't need to ADD a root to BirthDeath trees, and we can check what the root is without
checking a stored name for the root.

We also probably won't make birth-death trees from a list of edges?

And, to find an edge from two nodes (n1,n2), we probably just refer to the edge by
the bottom node, plus whether the edge points up or down?

So, this requires generalizing the Node and Edge types.
I guess we wouldn't NEED an IntMap IntEdge.

data Node = Root Time Tree | NonRoot Tree

parent (Root _ _) = Nothing
parent node@(NonRoot (Tree _ _ prev)) = case prev of Right node2 -> node2
                                                     Left time -> Root time node

PROBLEM: currently I've put IntSet into the definition of Tree.

class Tree t where
   type Node t
   type Edge t
   nodes :: t -> [Node] -- or IntSet?
   neighbors :: t -> Node -> [Node] -- or Array Node?
   outEdges :: t -> Node -> [Edge] 
-}
