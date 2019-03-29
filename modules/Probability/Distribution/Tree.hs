module Probability.Distribution.Tree where

import Tree
import Probability.Random
import Probability.Distribution.Uniform

xrange start end | start < end = start:xrange (start+1) end
                 | otherwise   = []

pick_index 0 (h:t) = (h,t)
pick_index 0 [] = error "Trying to pick from empty list!"
pick_index i (h:t) = let (x, t2) = pick_index (i-1) t
                     in (x, h:t2)

remove_one [] = error "Cannot remove one from empty list"
remove_one list = do i <- sample $ uniform_int 0 (length list-1)
                     return $ pick_index i list

random_tree_edges [l1] _         = return []
random_tree_edges [l1,l2] _      = return [(l1,l2)]
random_tree_edges leaves internal = do (l1,leaves')  <- remove_one leaves
                                       (l2,leaves'') <- remove_one leaves'
                                       let (i:internal') = internal
                                       other_edges <- random_tree_edges (i:leaves'') internal'
                                       return $ [(l1,i),(l2,i)]++other_edges

random_tree 1 = return $ Tree (listArray' [[]]) (listArray' []) 1 0
random_tree n = do let num_nodes = 2*n-2
                   edges <- random_tree_edges [0..n-1] [n..num_nodes-1]
                   -- This flipping is suppose flip edges from (internal,leaf) -> (leaf, internal)
                   let maybe_flip (x,y) | (y<x)     = (y,x)
                                        | otherwise = (x,y)
                   -- Then the sorting is supposed order edges like (0,_), (1,_), (2,_)
                   -- in order to assign leaf branches the names 0..n-1
                   let sorted_edges = quicksortWith (\(leaf,internal) -> leaf) $ map maybe_flip edges
                   return $ tree_from_edges num_nodes sorted_edges

modifiable_tree mod tree = Tree (listArray' nodes) (listArray' branches) (numNodes tree) (numBranches tree) where
    nodes =    [ map mod (edgesOutOfNode tree n) | n <- xrange 0 (numNodes tree) ]
    branches = [ (mod s, mod i, mod t, mod r) | b <- xrange 0 (numBranches tree * 2), let (s,i,t,r) = nodesForEdge tree b]


uniform_topology_pr 1 = doubleToLogDouble 1.0
uniform_topology_pr 2 = doubleToLogDouble 1.0
uniform_topology_pr n = uniform_topology_pr (n-1) / (doubleToLogDouble $ intToDouble $ 2*n-5)

modifiable_tree_pdf n value rv = let mod v = rv `seq` modifiable v
                                     tree = modifiable_tree mod value
                                 in (tree, uniform_topology_pr n)

uniform_topology n = Distribution (\tree-> uniform_topology_pr n) (no_quantile "uniform_topology") (RandomStructureAndPDF (modifiable_tree_pdf n) (random_tree n)) (TreeRange n)
