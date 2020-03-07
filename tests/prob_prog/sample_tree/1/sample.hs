import           Probability

import           Tree
import           Tree.Newick

main = random $ do
    tree <- uniform_topology 5
    return ["tree" %=% write_newick tree]

