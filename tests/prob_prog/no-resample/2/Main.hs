import           Probability

model x = do

    n <- sample $ geometric 0.33

    y <- if n > 1 then sample $ normal 0 1 else sample $ exponential 1

    observe x $ normal y 1

    return ["n" %=% n, "y" %=% y]

main = return $ model 3
