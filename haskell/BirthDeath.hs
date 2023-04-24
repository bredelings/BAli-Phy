{-# LANGUAGE RecursiveDo #-}
module BirthDeath where
-- See https://hackage.haskell.org/package/elynx-tree-0.3.2
-- See also Probability/Distribution/Tree/BirthDeath.hs

import Probability

type Time = Double

-- If we're going to make the tree traversable or foldable, then we'd need to
-- store some data on each node.  Should we consider the Time to be such data?
-- Maybe, but we could, in theory annotate the tree with additional data.

data Event = Start1 Tree |
             Start2 Tree Tree |
             Birth Tree Tree |
             Sample Tree |
             Death |
             Finish

-- We could also include events Start1 and Start2.

data Tree = Tree Time Event (Maybe Tree)



{- * If we store the branch duration, then we could shift a subtree by shrinking one branch.
       We could shrink one branch and lengthen child branches to move just one node.
       But we would still probably have to recalculate the node times after.

   * If we store the end time, then we could move a single node without having to adjust other nodes.
       But if we want to shift an entire subtree, then we have to visit all the times and update them.
-}

bd1 lambda mu t1 t2 prev = do
  -- Get the time of the next event
  t <- (t1 +) <$> (sample (exponential (1/(lambda+mu))))

  -- Determine if its a birth or death
  death <- sample (bernoulli (mu/(lambda+mu)))

  if t > t2 then
     return $ Tree t2 Finish prev
  else if death == 1 then
     return $ Tree t Death prev
  else
      do rec tree1 <- bd1 lambda mu t t2 (Just node)
             tree2 <- bd1 lambda mu t t2 (Just node)
             let node = Tree t (Birth tree1 tree2) prev
         return node


bd lambda mu t1 t2 = do
  rec next <- bd1 lambda mu t1 t2 (Just tree)
      let tree = Tree t1 (Start1 next) Nothing
  return tree

bd2 lambda mu t1 t2 = do
  rec next1 <- bd1 lambda mu t1 t2 (Just tree)
      next2 <- bd1 lambda mu t1 t2 (Just tree)
      let tree = Tree t1 (Start2 next1 next2) Nothing
  return tree


type Node = Tree

data Edge = Edge Node Bool

--- Hmm... in both of these cases, we walk the tree and sum an integer for each node.

numBranches (Tree _ (Start1 next) _) = 1 + numBranches next
numBranches (Tree _ (Start2 next1 next2) _) = 1 + numBranches next1 + numBranches next2
numBranches (Tree _ Death _ ) = 1
numBranches (Tree _ Finish _) = 1
numBranches (Tree _ (Sample next) _ ) = 1 + numBranches next
numBranches (Tree _ (Birth next1 next2) _) = 1 + numBranches next1 + numBranches next2

numLeaves (Tree _ (Start1 next) _) = numLeaves next
numLeaves (Tree _ (Start2 next1 next2) _) = numLeaves next1 + numLeaves next2
numLeaves (Tree _ Death _) = 1
numLeaves (Tree _ Finish _) = 1
numLeaves (Tree _ (Sample next) _ ) = numLeaves next
numLeaves (Tree _ (Birth next1 next2) _) = numLeaves next1 + numLeaves next2
         
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
