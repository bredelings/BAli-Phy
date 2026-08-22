module Probability.Distribution.PhyloCTMC.Properties where

import Bio.Alignment
import Bio.Alphabet  -- for type Alphabet
import Numeric.LinearAlgebra

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import SModel.Likelihood.CLV
import SModel.Property
import Numeric.Log

class PhyloCTMCProperties a where
      prop_anc_cat_states :: a -> IntMap ComponentStateSequence
      prop_likelihood :: a -> Log Double
      prop_smodel_properties :: a -> PropertyMap
      prop_smodel_conditions :: a -> ConditionMap
