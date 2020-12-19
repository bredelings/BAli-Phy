{-# LANGUAGE RecursiveDo #-}
import           Probability
import           Tree
import           Tree.Newick

main = sample $ do
    tree <- uniform_topology 5
    let rtree = add_root tree 0

    let ps    = map (show . parentNode rtree) [0 .. 5]

    rec let mu node = case parentNode rtree node of
                Nothing   -> 0.0
                Just node -> xs !! node
        xs <- independent [ normal (mu node) 1.0 | node <- nodes rtree ]
    -- can we _observe_ from this? -- why or why not?

    return ["tree" %=% write_newick rtree, "xs" %=% xs, "ps" %=% ps]
