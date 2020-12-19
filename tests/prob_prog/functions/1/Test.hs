import           Probability

cumsum xs = go 0 xs  where
    go acc []       = []
    go acc (y : ys) = let acc' = acc + y in acc' : go acc' ys

main = sample $ do
    z1 <- iid 3 (poisson 1.0)
    let z2 = [1 .. 3]
    let z3 = replicate 3 0

    -- print(z1)
    -- print(z2)
    -- print(z3)

    let x  = [1, 2, 3]

    let y1 = cumsum x

    -- we should be able to do functions of data structures with random fields
    let w1 = cumsum z1

    return ["z1" %=% z1, "z2" %=% z2, "z3" %=% z3, "x" %=% x, "y1" %=% y1, "w1" %=% w1]
