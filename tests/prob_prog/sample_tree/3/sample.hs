{-# LANGUAGE RecursiveDo #-}
module Model where

import           Probability
import           Tree
import           Tree.Newick
import           Control.Monad.Fix -- should be unneeded

model = do
    tree <- prior $ uniformTopology 5
    let rtree = addRoot 0 tree

    let ps    = map (show . parentNode rtree) [0 .. 5]

    rec let mu node = case parentNode rtree node of
                Nothing   -> 0.0
                Just node -> xs !! node
        xs <- prior $ independent [ normal (mu node) 1.0 | node <- nodes rtree ]
    -- can we _observe_ from this? -- why or why not?

    return ["tree" %=% writeNewick rtree, "xs" %=% xs, "ps" %=% ps]

main logDir = do
  return model
