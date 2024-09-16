module Probability.Distribution.Tree.Modifiable where

import           Tree
import           Probability.Random (triggered_modifiable_structure)

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

modifiable_tree :: (forall a.a->a) -> Tree -> Tree
modifiable_tree modf tree@(Tree (Forest (Graph nodes0 branches0 na ea ta))) = (Tree (Forest (Graph nodesMap branchesMap na ea ta))) where
    nodesMap = fmap (\(Node node branches_out) -> Node node (modf branches_out)) nodes0
    branchesMap = fmap (\(Edge s t b) -> Edge (modf s) (modf t) b ) branches0

-- The *triggered* tree is lazy: when we access anything that is modifiable, it triggers all effects,
-- which includes forcing all the modifiables in the *untriggered* tree.

-- We don't want to force all fields of the tree when _any_ tree field is accessed, only when a _random_ field is accessed.
-- This is why triggered tree still uses 'tree' as input to 'modifiable_tree'.
triggered_modifiable_tree = triggered_modifiable_structure modifiable_tree

-- leaves   nodes  branches
-- 1        1      0
-- 2        3      2
-- 3        5      4
-- 4        7      6
modifiable_rooted_tree :: (forall a.a -> a) -> WithRoots Tree -> WithRoots Tree
modifiable_rooted_tree modf (WithRoots tree [root_node] _) = add_root root_node $ modifiable_tree modf tree
-- Is it still true that we need the root node to have a constrant degree?

triggered_modifiable_rooted_tree = triggered_modifiable_structure modifiable_rooted_tree

-- A uniform-ordered-history distribution would need to augment nodes with an Int order, instead of a Double order.

-- maybe modf has type (forall a . a -> a)?
-- we should be able to apply it to both Int and Double...
modifiable_time_tree :: (forall a.a -> a) -> WithNodeTimes (WithRoots Tree) -> WithNodeTimes (WithRoots Tree)
modifiable_time_tree modf (WithNodeTimes rooted_tree' times') = WithNodeTimes rooted_tree times where
    rooted_tree = modifiable_rooted_tree modf rooted_tree'
    maybe_modf :: Int -> a -> a
    maybe_modf node x | node < numLeaves rooted_tree'   = x
                      | otherwise                       = modf x
    times     = IntSet.fromList [0..numNodes rooted_tree'-1] & IntMap.fromSet (\node -> maybe_modf node (times' IntMap.! node))

triggered_modifiable_time_tree = triggered_modifiable_structure modifiable_time_tree

