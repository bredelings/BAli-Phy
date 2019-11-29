import Probability
import Tree

main = random $ do
  tree <- uniform_topology 5
  let rtree = add_root tree 0

  let ps = map (show . parentNode rtree) [0..5]

  let mu xs Nothing  = 0.0
      mu xs (Just p) = xs!!p
  xs <- mfix (\xs -> independent [normal (mu xs p) 1.0  | n <- nodes rtree, let p = parentNode rtree n])

  return $ log_all [ "tree" %=% write_newick rtree , "xs" %=% xs, "ps" %=% ps ]
