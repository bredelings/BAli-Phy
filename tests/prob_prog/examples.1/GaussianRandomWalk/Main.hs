-- See https://github.com/probmods/webppl/blob/dev/examples/gaussianRandomWalk.wppl
import Probability

markov next state0 = lazy $ do state1 <- next state0
                               states <- markov next state1
                               return (state0:states)

transition xs = independent [normal x 10.0 | x <- xs]

gaussianRandomWalk n dim = do
  state0 <- iid dim (normal 200.0 1.0)
  states <- markov transition state0
  return $ take n states

model = do
  states <- gaussianRandomWalk 100 2
  return ["states" %=% states]

main = do
  mcmc model
