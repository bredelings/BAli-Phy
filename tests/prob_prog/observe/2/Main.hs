import           Probability.Random
import           Probability.Distribution.Normal

observe_data z' = do
    x <- normal 0.0 1.0
    y <- normal x   1.0
    z' ~> normal y 1.0
    return ["x" %=% x, "y" %=% y]

main = do
  let model = observe_data 1.0

  mcmc model
