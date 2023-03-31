import           Probability

random_walk next x0 = lazy $ do
    x1 <- next x0
    xs <- random_walk next x1
    return (x0 : xs)

random_walk_n n next x0 = do
    xs <- random_walk next x0
    return (take n xs)

model = do

    p <- beta 10.0 1.0

    n <- geometric p

    q <- cauchy 0.0 1.0

    x <- iid 10 (normal 0.0 1.0)

    c <- iid 10 (categorical (replicate 10 0.1))

    -- Random array indices.
    -- You can't do this in BUGS: it makes a dynamic graph!
    let w = [ x !! (c !! i) | i <- [0 .. 9] ]

    -- y[i] depends on x[i]
    y <- independent [ normal (x !! i) 1.0 | i <- [0 .. 9] ]

    -- Brownian-bridge-like
    z <- random_walk_n 10 (\mu -> normal mu 1.0) 0.0

    2.0 ~> normalDist (last z) 1.0

    return ["p" %=% p, "n" %=% n, "q" %=% q, "x" %=% x, "w" %=% w, "y" %=% y, "z" %=% z]

main = do
  mcmc model
