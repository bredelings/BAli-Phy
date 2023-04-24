module Probability.Distribution.Tree.BirthDeath
    ( bd1
    , bd2
    , all_tips
    , surviving_tips
    , to_newick
    )
where

import           Probability

data Tree = Start1 Tree |
            Start2 Tree Tree |
            Birth Tree Tree |
            Death |
            Finish

-- Start birth-death process at time t0 and run until
bd' lambda mu t1 t0 = do
    t'    <- (t0 +) `liftM` sample $ exponential (1.0 / (lambda + mu))
    death <- sample $ bernoulli (mu / (lambda + mu))
    if t' > t1 then
        return (t1, Finish)
    else if death == 1 then
        return (t', Death)
    else
        do
          tree1 <- bd' lambda mu t1 t'
          tree2 <- bd' lambda mu t1 t'
          return (t', Birth tree1 tree2)

bd2 lambda mu t1 = do
    tree1 <- bd' lambda mu t1 0.0
    tree2 <- bd' lambda mu t1 0.0
    return (0.0, Start2 tree1 tree2)

bd1 lambda mu t1 = do
    tree1 <- bd' lambda mu t1 0.0
    return (0.0, Start1 tree1)


surviving_tips (_, Finish      ) = 1
surviving_tips (_, Death       ) = 0
surviving_tips (_, Birth t1 t2 ) = surviving_tips t1 + surviving_tips t2
surviving_tips (_, Start2 t1 t2) = surviving_tips t1 + surviving_tips t2
surviving_tips (_, Start1 t    ) = surviving_tips t

all_tips (_, Finish      ) = 1
all_tips (_, Death       ) = 1
all_tips (_, Birth t1 t2 ) = all_tips t1 + all_tips t2
all_tips (_, Start2 t1 t2) = all_tips t1 + all_tips t2
all_tips (_, Start1 t    ) = all_tips t

to_newick (t0, Death                ) = ""
to_newick (t0, Finish               ) = ""
to_newick (t0, (Start1 tree@(t1, _))) = concat ["(", to_newick tree, ":", show (t1 - t0), ");"]
to_newick (t0, (Start2 tree1@(t1, _) tree2@(t2, _))) = concat ["(", to_newick tree1, ":", show l1, ",", to_newick tree2, ":", show l2, ");"]
  where
    l1 = t1 - t0
    l2 = t2 - t0
to_newick (t0, (Birth tree1@(t1, _) tree2@(t2, _))) = concat ["(", to_newick tree1, ":", show l1, ",", to_newick tree2, ":", show l2, ")"]
  where
    l1 = t1 - t0
    l2 = t2 - t0
