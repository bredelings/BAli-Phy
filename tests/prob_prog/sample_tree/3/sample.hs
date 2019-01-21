import Distributions
import Tree

main = do
  tree <- sample $ uniform_topology 5
  let rtree = add_root tree 0

  let ps = map (show . parentNode rtree) [0..5]

  (_,xs) <- mfix \ ~(mu,xs) -> do let mu' Nothing  = 0.0
                                      mu' (Just p) = xs!!p
                                  xs' <- Lazy $ sample $ list [normal (mu p) 1.0  | n <- nodes rtree, let p = parentNode rtree n]
                                  return (mu', xs')

  return $ log_all [ write_newick rtree %% "tree", xs %% "xs", ps %% "ps" ]
