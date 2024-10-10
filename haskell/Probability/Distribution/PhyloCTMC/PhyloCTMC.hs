module Probability.Distribution.PhyloCTMC.PhyloCTMC where

import SModel.Simple

data PhyloCTMC t a s  = PhyloCTMC t a s Double

phyloCTMC tree alignment smodel scale = PhyloCTMC tree alignment smodel scale

