module Probability.Distribution.PhyloCTMC.FixedA.Properties where

import Bio.Sequence -- for sequence_to_indices
import Bio.Alignment
import Bio.Alphabet  -- for type Alphabet
import Data.Matrix

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import SModel.Likelihood.CLV
import Numeric.LogDouble

import Probability.Distribution.PhyloCTMC.Properties


-- 0 - subst_root
-- 1 - transition_ps
-- 2 - CLVs
-- 3 - likelihood
-- 4 - alphabet
-- 5 - n_states
-- 6 - n_base_models
-- # - ?ancestral states         -- used through haskell, not parameters.cc

-- Some of these things could be accessed through the distribution arguments:
-- 4. alphabet
-- 5. n_states
-- 6. n_base_models


data PhyloCTMCPropertiesFixedA = PhyloCTMCPropertiesFixedA {
      prop_fixed_a_subst_root :: Int,
      prop_fixed_a_transition_ps :: IntMap (EVector (Matrix Double)),
      prop_fixed_a_cond_likes :: IntMap CondLikes,
      prop_fixed_a_likelihood :: LogDouble,
      prop_fixed_a_alphabet :: Alphabet,
      prop_fixed_a_n_states :: Int,
      prop_fixed_a_n_base_models :: Int,

      prop_fixed_a_anc_cat_states :: IntMap VectorPairIntInt
    }


instance PhyloCTMCProperties PhyloCTMCPropertiesFixedA where
    prop_likelihood = prop_fixed_a_likelihood
    prop_anc_cat_states = prop_fixed_a_anc_cat_states

