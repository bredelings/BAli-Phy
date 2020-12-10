import           Probability

model = do
    n <- geometric 0.33
    y <- if n > 1 then normal 0.0 1.0 else exponential 1.0
    let loggers = ["n" %=% n, "y" %=% y]
    return (y,loggers)

main = do
    (y,loggers) <- random $ model
    observe (normal y 1.0) 3.0
    return loggers
