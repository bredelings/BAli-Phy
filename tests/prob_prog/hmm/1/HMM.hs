module HMM where

-- See https://github.com/probmods/webppl/blob/dev/examples/hmm.wppl
import Probability

transition_matrix s = sample $ categorical $ [[0.7, 0.3], [0.3, 0.7]] !! s

emission_matrix s = categorical $ [[0.9, 0.1], [0.1, 0.9]] !! s

markov f state0 = lazy $ do state1 <- f state0
                            states <- markov f state1
                            return (state0:states)

observations =  [1,1,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,0,1,1,1,0,1,0,0,0,0,1,1,1,1,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,1,1] :: [Int]

true_hidden_states = [1,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,0,0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1] :: [Int]

n_diffs []     []                 = 0 :: Int
n_diffs (x:xs) (y:ys) | x == y    =     n_diffs xs ys
                      | otherwise = 1 + n_diffs xs ys

hmm emission states = independent $ map emission states

model n = do
  hidden_states <- take n <$> markov transition_matrix 1
  observe observations $ hmm emission_matrix hidden_states
  return ["hidden_states" %=% hidden_states,
          "diff-true" %=% n_diffs hidden_states true_hidden_states,
          "diff-obs" %=% n_diffs hidden_states observations]

main = do
  return $ model 100
