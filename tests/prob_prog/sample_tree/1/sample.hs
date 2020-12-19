import           Probability

import           Tree
import           Tree.Newick

main = sample $ do
    tree <- uniform_topology 5
    return ["tree" %=% write_newick tree]

