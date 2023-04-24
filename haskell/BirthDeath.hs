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

edgesOutOfNode = node_out_edges

sourceNode :: Edge a -> Node a
sourceNode (Edge node ToRoot) = Right node
sourceNode (Edge (Next _ _ parent) FromRoot) = parent

targetNode :: Edge a -> Node a
targetNode (Edge node FromRoot) = Right node
targetNode (Edge (Next _ _ parent) ToRoot) = parent

reverseEdge :: Edge a -> Edge a
reverseEdge (Edge node FromRoot) = Edge node ToRoot
reverseEdge (Edge node ToRoot) = Edge node FromRoot

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

edgesTowardNode node = fmap reverseEdge $ edgesOutOfNode node

-- edgeForNodes == ???

nodeDegree = length . edgesOutOfNode
neighbors = fmap targetNode . edgesOutOfNode
sourceIndex edge = 0 -- FIXME!!
edgesBeforeEdge edge = let source = sourceNode edge
                           index = sourceIndex edge
                       in fmap reverseEdge $ removeElement index $ edgesOutOfNode source

edgesAfterEdge = fmap reverseEdge . edgesBeforeEdge . reverseEdge

undirected_edges tree = [ Edge node ToRoot | Right node <- nodes tree ]

root tree = Left tree
numBranches tree = length $ undirected_edges tree

node_time node (Left (Start1 t _)) = t
node_time node (Left (Start2 t _ _)) = t
node_time node (Right (Next t _ _)) = t

branch_duration edge = abs (node_time (sourceNode edge) - node_time (targetNode edge))

branch_length edge = branch_duration edge

nodes node@(Left (Start1 _ next)) = node:nodes (Right next)
nodes node@(Left (Start2 _ next1 next2)) = node:nodes (Right next1) ++ nodes (Right next2)
nodes node@(Right (Next _ (Birth next1 next2) _)) = node:nodes (Right next1) ++ nodes (Right next2)
nodes node@(Right (Next _ (Sample next) _)) = node:nodes (Right next)
nodes node@(Right (Next _ Finish _)) = [node]
nodes node@(Right (Next _ Death _)) = [node]

leaf_nodes t = filter is_leaf_node (nodes t)
internal_nodes t = filter is_internal_node (nodes t)

numLeaves tree = length $ leaf_nodes tree

tree_length tree = sum [ branch_length b | b <- undirected_edges tree ]

allEdgesAfterEdge b = b:concatMap allEdgesAfterEdge (edgesAfterEdge b)
allEdgesFromNode n = concatMap allEdgesAfterEdge (edgesOutOfNode n)

