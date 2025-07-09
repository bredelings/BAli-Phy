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
      prop_subst_root :: a -> Int
      prop_transition_ps :: a -> IntMap (EVector (Matrix Double))
      prop_cond_likes :: a -> IntMap CondLikes
      prop_anc_seqs :: a -> AlignedCharacterData
      prop_likelihood :: a -> LogDouble
      prop_alphabet :: a -> Alphabet
      prop_n_states :: a -> Int
      prop_n_base_models :: a -> Int


