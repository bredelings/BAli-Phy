import           Probability

import           Tree
import           Tree.Newick

model = sample $ do
    tree <- uniform_topology 5
    return ["tree" %=% write_newick tree]

main = do
  mcmc model
