module LiStephens2003 where

import           Bio.Alignment
import           Bio.Alphabet
import           Probability
import           PopGen
import           System.Environment

filename = getArgs !! 0

sequence_data = load_alignment dna filename

model = do

    rho       <- log_laplace 0.01 1.0

    return rho

main = do

    rho <- random $ model

    sequence_data ~> li_stephens_2003 rho

    return ["rho" %=% rho ]
