module Demo2 where

import Probability

main = do

  xs <- sample $ iid 10 (normal 0.0 1.0)

  let ys = map (\x -> x*x) xs

  return $ log_all [xs %% "xs", ys %% "squares", sum ys %% "sum"]

