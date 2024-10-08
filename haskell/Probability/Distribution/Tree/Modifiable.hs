module Probability.Distribution.Tree.Modifiable where

import           Tree
import           Probability.Random (triggeredModifiableStructure)

import           Data.Array
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

{- Note: What the modifiable structure assumes.

   Our current modifiable tree structure requires that
   1. the name for the reverse edge doesn't change.
   2. the nodes and edges in the tree don't change.

   We could add `modf` in front of `r` to change (1).

   We could add `modf` in from of nodesMap and branchesMap to change (2).
   But we'd have to handle changes in the keysSet of node attributes (`na`) and edge attributes (`ea`).

   In both cases, adding more `modf` leads to:
   A. a slowdown
   B. a error saying that something isn't a modifiable value.  -}

modifiableTree :: (forall a.a->a) -> (Tree l) -> (Tree l)
modifiableTree modf tree@(Tree (Forest (Graph nodes0 branches0 labels na ea ta))) = (Tree (Forest (Graph nodesMap branchesMap labels na ea ta))) where
    nodesMap = fmap (\(Node node branches_out) -> Node node (modf branches_out)) nodes0
    branchesMap = fmap (\(Edge s t b) -> Edge (modf s) (modf t) b ) branches0

-- The *triggered* tree is lazy: when we access anything that is modifiable, it triggers all effects,
-- which includes forcing all the modifiables in the *untriggered* tree.

-- We don't want to force all fields of the tree when _any_ tree field is accessed, only when a _random_ field is accessed.
-- This is why triggered tree still uses 'tree' as input to 'modifiableTree'.
triggeredModifiableTree = triggeredModifiableStructure modifiableTree

-- leaves   nodes  branches
-- 1        1      0
-- 2        3      2
-- 3        5      4
-- 4        7      6
modifiableRootedTree :: (forall a.a -> a) -> WithRoots (Tree l) -> WithRoots (Tree l)
modifiableRootedTree modf (WithRoots tree [rootNode] _) = addRoot rootNode $ modifiableTree modf tree
-- Is it still true that we need the root node to have a constrant degree?

triggeredModifiableRootedTree = triggeredModifiableStructure modifiableRootedTree

---------- Coalescent time tree --------------

{- NOTE: Coalescent versus birth-death.

This function doesn't add a modifiable to the leaf node times.  Such a procedure
makes sense for the coalescent, where the leaf node times are an input to the coalescent.

For a birth-death-sampling tree, the times of the PAST samples are random, I think,
since they assume sampling at a certain rate?  And they can convert between leaf and internal too.

-}

{- NOTE: Coalescent versus uniform-ordered-history.

For a coalescent, the nodes have times, and we get an order by ordering those times.

For an ordered-history, we might need to add an integer order to each node to preserve
the order when we remove the times.
-}

modifiableTimeTree :: (forall a.a -> a) -> WithNodeTimes (WithRoots (Tree l)) -> WithNodeTimes (WithRoots (Tree l))
modifiableTimeTree modf (WithNodeTimes rootedTree' times') = WithNodeTimes rootedTree times where
    rootedTree = modifiableRootedTree modf rootedTree'
    maybeModf :: Int -> a -> a
    maybeModf node x | isLeafNode rootedTree' node = x
                     | otherwise                   = modf x
    times     = (IntMap.keysSet times') & IntMap.fromSet (\node -> maybeModf node (times' IntMap.! node))

triggeredModifiableTimeTree = triggeredModifiableStructure modifiableTimeTree
