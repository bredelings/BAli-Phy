import           Probability.Random
import           Probability.Distribution.Normal

observe_data x= do
    x ~> normal 0.0 1.0
    return []

main = do
  let model = observe_data 1.0

  mcmc model
