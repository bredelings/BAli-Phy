import           Probability

random_walk n x1 x2
    | n < 2 = error ("Random walk must have at least 2 points")
    | n == 2 = return [x1, x2]
    | otherwise = do
        dx <- normal 0.0 1.0
        let x3 = dx - x1 + (2.0 * x2)
        xs <- random_walk (n - 1) x2 x3
        return (x1 : xs)

main = random $ do
    x1 <- normal 0.0 1.0
    x2 <- normal x1 (sqrt $ 1.0 / 3.0)
    xs <- random_walk 100 x1 x2
    return ["x" %=% xs]
