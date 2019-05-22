import Probability

import Tree

main = random $ do
  tree <- sample $ uniform_topology 5
  return $ log_all [ write_newick tree %% "tree"]

