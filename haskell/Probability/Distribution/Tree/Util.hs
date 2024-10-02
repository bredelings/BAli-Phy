module Probability.Distribution.Tree.Util where

import Probability.Random
import Probability.Distribution.Uniform
import Probability.Distribution.Exponential
import Data.Maybe (isJust)
import Tree

type Time = Double
type Rate = Double

xrange start end | start < end = start : xrange (start + 1) end
                 | otherwise   = []

pickIndex _ []      = error "Trying to pick from empty list!"
pickIndex 0 (h : t) = (h, t)
pickIndex i (h : t) = let (x, t2) = pickIndex (i - 1) t in (x, h : t2)

removeOne []   = error "Cannot remove one from empty list"
removeOne [x]  = return (x,[])
removeOne list = do
    i <- sample $ uniform_int 0 (length list - 1)
    return $ pickIndex i list

remove n list | n < 0           = error $ "Trying to remove " ++ show n ++ "entries from list"
              | n > length list = return Nothing
              | otherwise       = Just <$> go n list
    where go 0 list = return ([],list)
          go n list = do (x , list_minus_1) <- removeOne list
                         (xs, list_minus_n) <- go (n - 1) list_minus_1
                         return (x:xs, list_minus_n)


possible = 1 :: LogDouble
impossible = 0 :: LogDouble
require p = if p then possible else impossible

shuffle [] = return []
shuffle xs = do
  (first,rest) <- removeOne xs
  restShuffled <- shuffle rest
  return (first:restShuffled)

parentBeforeChildPrs tree = [factor n | n <- getNodes tree, isJust (parentNode tree n)]
    where time = nodeTime tree
          factor n = case parentNode tree n of Just p  -> require $ time n <= time p

