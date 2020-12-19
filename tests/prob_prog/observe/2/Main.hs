import           Probability.Random
import           Probability.Distribution.Normal

main = do
    x <- sample $ normal 0.0 1.0
    y <- sample $ normal x   1.0
    1.0 ~> normal y 1.0
    return []
