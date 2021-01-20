import           Probability

random_walk next x0 = do
    x1 <- next x0
    xs <- random_walk next x1
    return (x0 : xs)

-- 20 element brownian bridge from 0.0 to 2.0
main = do
    walk <- random $ random_walk (\mu -> normal mu 1.0) 0.0

    let zs = take 19 walk

    2.0 ~> normal (last zs) 1.0

    return ["zs" %=% zs]
