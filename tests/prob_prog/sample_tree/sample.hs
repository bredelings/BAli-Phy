import Distributions

import Tree

main = do
  tree <- sample $ uniform_topology 5
  return $ log_all [ write_newick tree %% "tree"]

