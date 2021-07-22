import           Probability.Random
import           Probability.Distribution.Normal

model = do
  x <- normal 0.0 1.0
  y <- normal x   1.0
  return []

main = do
  mcmc model
