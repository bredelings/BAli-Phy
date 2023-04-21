import           Probability

import           Tree
import           Tree.Newick

model = do
    tree <- sample $ uniform_topology 5
    return ["tree" %=% write_newick (make_rooted tree)]

main = do
  mcmc model
