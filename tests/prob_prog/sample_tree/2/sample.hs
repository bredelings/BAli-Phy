module Model where

import           Probability
import           Tree
import           Tree.Newick
import           Control.Monad.Fix

model = do
    tree <- sample $ uniformTopology 5
    let rtree = addRoot 0 tree

    let ps    = map (show . parentNode rtree) [0 .. 5]

    let mu xs Nothing  = 0.0
        mu xs (Just p) = xs !! p
    xs <- mfix (\xs -> sample $ independent [ normal (mu xs p) 1.0 | n <- nodes rtree, let p = parentNode rtree n ])

    return ["tree" %=% writeNewick rtree, "xs" %=% xs, "ps" %=% ps]

main = do
  return model
