import Probability

import Tree

main = random $ do
  xs <- sample $ crp 2.0 10 2
  return $ log_all [ xs %% "xs"]

