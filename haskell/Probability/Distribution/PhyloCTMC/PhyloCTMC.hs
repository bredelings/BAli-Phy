module Probability.Distribution.PhyloCTMC.PhyloCTMC where

import SModel.Simple
import Tree

data PhyloCTMC t a s r where
    PhyloCTMC :: t -> a -> s -> Double -> PhyloCTMC t a s (IsReversible s)

phyloCTMC :: IsTree t => t -> a -> s -> Double -> PhyloCTMC (Rooted t) a s (IsReversible s)
phyloCTMC tree alignment smodel scale = PhyloCTMC (makeRooted tree) alignment smodel scale

