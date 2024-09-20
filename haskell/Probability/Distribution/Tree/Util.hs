module Probability.Distribution.Tree.Util where

import Probability.Random
import Probability.Distribution.Uniform
import Probability.Distribution.Exponential

xrange start end | start < end = start : xrange (start + 1) end
                 | otherwise   = []

pickIndex 0 (h : t) = (h, t)
pickIndex 0 []      = error "Trying to pick from empty list!"
pickIndex i (h : t) = let (x, t2) = pickIndex (i - 1) t in (x, h : t2)

removeOne []   = error "Cannot remove one from empty list"
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