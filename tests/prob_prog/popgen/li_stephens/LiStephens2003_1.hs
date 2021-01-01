module LiStephens2003 where

import           Bio.Alignment
import           Bio.Alphabet
import           Probability
import           PopGen
import           System.Environment

prior = do

    rho       <- log_laplace 0.01 1.0

    return rho

observe_data sequence_data = do

    rho <- sample $ prior

    sequence_data ~> li_stephens_2003 rho

    return ["rho" %=% rho ]

main = do
  let filename = getArgs !! 0
      sequence_data = load_alignment dna filename

  let model = observe_data sequence_data
  mcmc model
