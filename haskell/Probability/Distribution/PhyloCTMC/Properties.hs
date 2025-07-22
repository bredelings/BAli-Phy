module Probability.Distribution.PhyloCTMC.Properties where

import Bio.Sequence -- for sequence_to_indices
import Bio.Alignment
import Bio.Alphabet  -- for type Alphabet
import Data.Matrix

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import SModel.Likelihood.CLV
import Numeric.LogDouble

class PhyloCTMCProperties a where
      prop_anc_cat_states :: a -> IntMap VectorPairIntInt
      prop_likelihood :: a -> LogDouble
