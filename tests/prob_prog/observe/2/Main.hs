import           Probability.Random
import           Probability.Distribution.Normal

observe_data z' = do
    x <- normal 0 1
    y <- normal x 1
    z' ~> normalDist y 1
    return ["x" %=% x, "y" %=% y]

main = do
  let model = observe_data 1

  mcmc model
