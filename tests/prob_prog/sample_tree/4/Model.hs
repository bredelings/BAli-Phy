{-# LANGUAGE RecursiveDo #-}
module Model where

import           Probability
import           Tree
import           Tree.Newick
import qualified Data.Text as Text
import           Control.Monad.Fix -- should be unneeded

n_leaves = 3

allStrings = [ c : s | s <- "" : allStrings, c <- ['a'..'j'] ]

allTexts = fmap Text.pack allStrings

model = do
    tree <- sample $ uniform_time_tree 1.0 n_leaves
    let labels = take n_leaves $ zip [0..] allTexts
        ltree = addLabels labels tree
    let pr = uniform_time_tree_pr 1.0 n_leaves ltree

    let ps    = map (\n -> show (n, parentNode tree n)) [0 .. numNodes tree - 1]

    rec let mu node = case parentNode tree node of
                Nothing   -> 0.0
                Just node -> xs !! node
        xs <- sample $ independent [ normal (mu node) 1.0 | node <- nodes tree ]
  -- can we _observe_ from this? -- why or why not?

    return ["tree" %=% writeNewick tree] --,"pr" %=% pr, "xs" %=% xs, "ps" %=% ps]

main = do
  return model
