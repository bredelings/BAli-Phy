-- See https://github.com/probmods/webppl/blob/dev/examples/hmm.wppl
import Probability

transition_matrix s = categorical ([[0.7, 0.3], [0.3, 0.7]]!!s)

emission_matrix s = categorical ([[0.9, 0.1], [0.1, 0.9]]!!s)

markov :: (a->Random a) -> a -> Random [a]
markov f state0 = do state1 <- f state0
                     states <- markov f state1
                     return (state0:states)

observations =  [1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,0,1,1,1,0,1,0,0,0,0,1,1,1,1,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,1,1]

true_hidden_states = [1,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,0,0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1]

n_diffs []     []                 = 0
n_diffs (x:xs) (y:ys) | x == y    =     n_diffs xs ys
                      | otherwise = 1 + n_diffs xs ys

hmm emission states = independent $ map emission states

model n = take n `liftM` markov transition_matrix 1

main = do
  hidden_states <- random $ model 100
  observations ~> hmm emission_matrix hidden_states
  return ["hidden_states" %=% hidden_states,
          "diff-true" %=% n_diffs hidden_states true_hidden_states,
          "diff-obs" %=% n_diffs hidden_states observations]

