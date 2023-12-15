module Probability.Distribution.PhyloCTMC.PhyloCTMC where

import SModel.Simple

data PhyloCTMC t a s r where
    PhyloCTMC :: t -> a -> s -> Double -> PhyloCTMC t a s (IsReversible s)

phyloCTMC tree alignment smodel scale = PhyloCTMC tree alignment smodel scale

