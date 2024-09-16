module Probability.Distribution.Tree.Util where

import Probability.Random
import Probability.Distribution.Uniform
import Probability.Distribution.Exponential

xrange start end | start < end = start : xrange (start + 1) end
                 | otherwise   = []

pick_index 0 (h : t) = (h, t)
pick_index 0 []      = error "Trying to pick from empty list!"
pick_index i (h : t) = let (x, t2) = pick_index (i - 1) t in (x, h : t2)

remove_one []   = error "Cannot remove one from empty list"
remove_one list = do
    i <- sample $ uniform_int 0 (length list - 1)
    return $ pick_index i list

remove_n 0 list = return ([], list)
remove_n n list = do
    (x , list_minus_1) <- remove_one list
    (xs, list_minus_n) <- remove_n (n - 1) list_minus_1
    return ((x : xs), list_minus_n)
