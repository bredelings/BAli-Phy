module Probability.Distribution.PhyloCTMC.PhyloCTMC where

data PhyloCTMC t a s = PhyloCTMC t a s Double

phyloCTMC tree alignment smodel scale = PhyloCTMC tree alignment smodel scale

data PhyloCTMC2 t a s = PhyloCTMC2 t a s Double

phyloCTMC2 tree alignment smodel scale = PhyloCTMC2 tree alignment smodel scale

