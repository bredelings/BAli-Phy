import           Probability

main = sample $ do

    xs <- iid 10 (normal 0.0 1.0)

    let ys = map (\x -> x * x) xs

    return ["xs" %=% xs, "squares" %=% ys, "sum" %=% sum ys]

