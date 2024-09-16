module Probability.Distribution.Tree.Modifiable where

import           Tree
import           Probability.Random (triggered_modifiable_structure)

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

