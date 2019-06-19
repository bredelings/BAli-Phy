module Test where

import Probability

main = random $ do xs <- sample $ iid 10 (categorical [0.1, 0.2, 0.3, 0.4])
                   return $ log_all [ xs %% "xs" ]
