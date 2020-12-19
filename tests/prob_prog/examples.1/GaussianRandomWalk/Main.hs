-- See https://github.com/probmods/webppl/blob/dev/examples/gaussianRandomWalk.wppl
import Probability

markov next state0 = do state1 <- next state0
                        states <- markov next state1
                        return (state0:states)

transition xs = independent [normal x 10.0 | x <- xs]

gaussianRandomWalk n dim = do
  state0 <- iid dim (normal 200.0 1.0)
  states <- markov transition state0
  return $ take n states

main = do
  states <- sample $ gaussianRandomWalk 100 2
  return ["states" %=% states]

