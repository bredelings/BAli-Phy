import Probability

import Tree

main = random $ do
  tree <- uniform_topology 5
  return $ log_all [ "tree" %=% write_newick tree ]

