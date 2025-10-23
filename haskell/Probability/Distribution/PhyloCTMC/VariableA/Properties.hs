module Probability.Distribution.PhyloCTMC.VariableA.Properties where

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
-- 7 - weightedfrequencymatrix   -- only variableA
-- 8 - sequence likelihoods      -- only variableA: used to get node CLVs for dists2 when aligning 2 sequences. 
-- # - ?ancestral states         -- used through haskell, not parameters.cc

-- Some of these things could be accessed through the distribution arguments:
-- 5. alphabet
-- 6. n_states
-- 7. n_base_models
-- 9-ish. sequences as EVector Int.


data PhyloCTMCPropertiesVariableA = PhyloCTMCPropertiesVariableA {
      prop_variable_a_subst_root :: Int,
      prop_variable_a_transition_ps :: IntMap (EVector (Matrix Double)),
      prop_variable_a_cond_likes :: IntMap CondLikes,
      prop_variable_a_likelihood :: LogDouble,
      prop_variable_a_alphabet :: Alphabet,
      prop_variable_a_n_states :: Int,
      prop_variable_a_n_base_models :: Int,
      prop_variable_a_get_weightedFrequencyMatrix :: IntMap (Matrix Double),   -- only variable A
      prop_variable_a_nodeCLVs :: IntMap (Maybe CondLikes),                    -- only variable A

      prop_variable_a_anc_cat_states :: IntMap VectorPairIntInt
    }


instance PhyloCTMCProperties PhyloCTMCPropertiesVariableA where
    prop_likelihood = prop_variable_a_likelihood
    prop_anc_cat_states = prop_variable_a_anc_cat_states
    
