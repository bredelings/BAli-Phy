module LiStephens2003 where

import           Bio.Alignment
import           Bio.Alphabet
import           Probability
import           PopGen
import           System.Environment

model sequence_data = do

    rho <- log_laplace 0.01 1.0

    sequence_data ~> li_stephens_2003 rho

    return ["rho" %=% rho ]

main = do
  [filename] <- getArgs
  let sequence_data = load_alignment dna filename
  mcmc $ model sequence_data
