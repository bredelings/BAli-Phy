import           Probability.Random
import           Probability.Distribution.Normal

main = do
    x <- random $ normal 0.0 1.0
    y <- random $ normal x   1.0
    1.0 ~> normal y 1.0
    return []
