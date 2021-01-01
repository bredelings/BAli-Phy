import           Probability.Random
import           Probability.Distribution.Normal

observe_data z' = do
    x <- sample $ normal 0.0 1.0
    y <- sample $ normal x   1.0
    z' ~> normal y 1.0
    return []

main = do
  let model = observe_data 1.0

  mcmc model
