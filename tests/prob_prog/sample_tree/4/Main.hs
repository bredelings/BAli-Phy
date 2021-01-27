{-# LANGUAGE RecursiveDo #-}
import           Probability
import           Tree
import           Tree.Newick

n_leaves = 3

allStrings = [ c : s | s <- "" : allStrings, c <- ['a','b','c','d','e','f','g','h','i','j'] ]

model = sample $ do
    tree <- uniform_time_tree 1.0 n_leaves
    let ltree = add_labels (take n_leaves allStrings) tree
    let pr = uniform_time_tree_pr 1.0 n_leaves ltree

    let ps    = map (\n -> show (n, parentNode tree n)) [0 .. numNodes tree - 1]

    rec let mu node = case parentNode tree node of
                Nothing   -> 0.0
                Just node -> xs !! node
        xs <- independent [ normal (mu node) 1.0 | node <- nodes tree ]
  -- can we _observe_ from this? -- why or why not?

    return ["tree" %=% write_newick tree] --,"pr" %=% pr, "xs" %=% xs, "ps" %=% ps]

main = do
  mcmc model
