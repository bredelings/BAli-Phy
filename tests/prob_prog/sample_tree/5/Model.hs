{-# LANGUAGE RecursiveDo #-}
module Model where

import           Probability
import           Tree
import           Tree.Newick
import qualified Data.Text as Text
import           Control.Monad.Fix -- should be unneeded
import qualified Data.IntMap as IntMap

nLeaves = 5

allTexts = fmap Text.pack allStrings

allStrings = [ c : s | s <- "" : allStrings, c <- ['a'..'j'] ]

model taxa = do
    let lambda = 1
    tree <- sample $ yule taxa lambda

    let rootValue = 0

    rec let meanFor node = case parentNode tree node of
                             Nothing   -> rootValue
                             Just node -> xs IntMap.! node
        xs <- sample $ independentMap (getNodesSet tree) (\node -> sample $ normal (meanFor node) 1)

    return ["tree" %=% writeNewick tree,
            "xs" %=% xs]

main logDir = do
  let taxa = take nLeaves allTexts
             
  return $ model taxa
