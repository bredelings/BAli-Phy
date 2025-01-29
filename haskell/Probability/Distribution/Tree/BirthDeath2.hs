{-# LANGUAGE RecursiveDo #-}
module Probability.Distribution.Tree.BirthDeath2 where

import Probability

type Time = Double
data Tree = Tree Int Time [Tree] (Maybe Tree)

nodeTime (Tree _ t _ _) = t
setParent p (Tree n t children _) = (Tree n t children (Just p))

sampleBirthDeathTree t b d s = do
  delta <- sample $ exponential (1/(b + d + s))
  u <- uniqueInt
  r <- sample $ uniform 0 (d + s + b)
  if delta > t
  then return (Tree u 0 [] Nothing) -- survives until present, and is observed.
  else do
      let t2 = t - delta
      if r < d
      then return (Tree u t2 [] Nothing) -- goes extinct at time t2
      else
          if r < d + s
          then do
            child <- sampleBirthDeathTree t2 b d s
            let node = Tree u t2 [setParent node child] Nothing -- a sampling event
            return node
          else do
            left <- sampleBirthDeathTree t2 b d s
            right <- sampleBirthDeathTree t2 b d s
            let node = Tree u t2 (setParent node <$> [left,right]) Nothing
            return node

sampleBirthDeathTree2 t b d s = do
    u <- uniqueInt
    left <- sampleBirthDeathTree t b d s
    right <- sampleBirthDeathTree t b d s
    let node = Tree u t (setParent node <$> [left, right]) Nothing
    return node


instance Show Tree where
    show (Tree node t children Nothing) = "(Tree " ++ show node ++ " " ++ show t ++ " " ++ show children ++ " ROOT)"
    show (Tree node t children (Just p)) = "(Tree " ++ show node ++ " " ++ show t ++ " " ++ show children ++ " " ++ show (nodeTime p) ++ ")"

survivingTips (Tree node t [] _) | t <= 0  = [node]
                                 | otherwise = []
survivingTips (Tree node t children _) = concat [survivingTips child | child <- children]

allTips (Tree node t [] _) = [node]
allTips (Tree node t children _) = concat [allTips child | child <- children]

sampledNodes (Tree node t [] _) = [node]
sampledNodes (Tree node t [child] _) = [node] ++ sampledNodes child
sampledNodes (Tree node t children _) = concat [sampledNodes child | child <- children]

brLen t Nothing = ""
brLen t (Just p) = ":" ++ show (nodeTime p - t)

toNewickNode (Tree node t [] p) = show node ++ brLen t p
toNewickNode (Tree node t [child] p) = "(" ++ toNewickNode child  ++ ")" ++ show node ++ brLen t p
toNewickNode (Tree node t children p) = "(" ++ intercalate "," [toNewickNode child | child <- children] ++ ")" ++ brLen t p

toNewick tree = toNewickNode tree ++ ";"
