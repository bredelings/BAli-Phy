module Probability.Distribution.PhyloCTMC.PhyloCTMC where

import SModel.Simple
import Reversible
import Tree -- for isRooted, makeRooted, setRoot

data PhyloCTMC t a s = PhyloCTMC t a s Double

phyloCTMC :: (IsTree t, CheckReversible s) => t -> a -> s -> Double -> PhyloCTMC (Rooted t) a s
phyloCTMC tree alignment smodel scale = if isReversible smodel
                                        then PhyloCTMC (makeRooted tree) alignment smodel scale
                                        else case isRooted tree of
                                               Unrooted -> error "phyloCTMC: the tree is unrooted, but the model is not reversible!"
                                               Rooted -> PhyloCTMC tree alignment smodel scale
