import           Probability.Random
import           Probability.Distribution.Normal

prior = do
  x <- normal 0.0 1.0
  y <- normal x   1.0
  return (x,y)

model = do
  (x,y) <- sample $ prior
  return []

main = do
  mcmc model
