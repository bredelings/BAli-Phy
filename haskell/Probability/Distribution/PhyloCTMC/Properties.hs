module Probability.Distribution.PhyloCTMC.Properties where

import Bio.Sequence -- for sequence_to_indices
import Bio.Alignment
import Bio.Alphabet  -- for type Alphabet
import Numeric.LinearAlgebra

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import SModel.Likelihood.CLV
import SModel.Property
import Numeric.LogDouble

class PhyloCTMCProperties a where
      prop_anc_cat_states :: a -> IntMap ComponentStateSequence
      prop_likelihood :: a -> LogDouble
      prop_smodel_properties :: a -> PropertyMap
