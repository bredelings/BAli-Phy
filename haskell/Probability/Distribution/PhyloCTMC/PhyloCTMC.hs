module Probability.Distribution.PhyloCTMC.PhyloCTMC where

data PhyloCTMC t a s = PhyloCTMC t a s Double

phyloCTMC tree alignment smodel scale = PhyloCTMC tree alignment smodel scale

