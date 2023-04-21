import           Probability.Random
import           Probability.Distribution.Normal

observe_data x = do
    observe x $ normal 0 1
    return []

main = do
  let model = observe_data 1

  mcmc model
