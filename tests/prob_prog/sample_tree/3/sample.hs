import Probability
import Tree

main = random $ do
  tree <- uniform_topology 5
  let rtree = add_root tree 0

  let ps = map (show . parentNode rtree) [0..5]

  rec let mu Nothing  = 0.0
          mu (Just p) = xs!!p
      xs <- independent [normal (mu parent_node) 1.0  | n <- nodes rtree, let parent_node = parentNode rtree n]

  return [ "tree" %=% write_newick rtree, "xs" %=% xs, "ps" %=% ps ]
