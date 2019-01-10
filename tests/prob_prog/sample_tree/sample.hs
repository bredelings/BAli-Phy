import Distributions

import Tree

main = do
  tree <- sample $ uniform_topology 5
  return $ log_all [ (write_newick 0 tree) %% "tree"]

