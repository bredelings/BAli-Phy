module Probability.Distribution.PhyloCTMC.Properties where

import Bio.Sequence -- for sequence_to_indices
import Bio.Alignment
import Bio.Alphabet  -- for type Alphabet
import Data.Matrix

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import SModel.Likelihood.CLV
import Numeric.LogDouble

-- 0 - subst_root
-- 1 - transition_ps
-- 2 - CLVs
-- 3 - ?ancestral states         -- used through haskell, not parameters.cc
-- 4 - likelihood
-- 5 - weightedfrequencymatrix
-- 6 - state -> letter           -- unused
-- 7 - sequence likelihoods      -- used to get node CLVs for dists2 when aligning 2 sequences.
-- 8 - alphabet
-- 9 - n_states
-- 10 - n_base_models
-- 11 - ?parsimony -- used through haskell, not parameters.cc

-- Some of these things could be accessed through the distribution arguments:
-- 6. state -> letter
-- 7. sequences as EVector Int
-- 8. alphabet
-- 9. n_states
-- 10. n_base_models


data PhyloCTMCProperties = PhyloCTMCProperties {
      prop_subst_root :: Int,
      prop_transition_ps :: IntMap (EVector (Matrix Double)),
      prop_cond_likes :: IntMap CondLikes,
-- Probably this shold be UnalignedCharacterData for variable-alignment models.
-- Is that slower?
      prop_anc_seqs :: AlignedCharacterData,
      prop_likelihood :: LogDouble,
      prop_get_weighted_frequency_matrix :: IntMap (Matrix Double), -- only variable A
      prop_smap :: EVector Int,
      prop_nodeCLVs :: IntMap (Maybe CondLikes),                    -- only variable A
      prop_alphabet :: Alphabet,
      prop_n_states :: Int,
      prop_n_base_models :: Int,
      prop_n_muts :: Int                                            -- This shouldn't be here.
    }


{- For FixedA.hs, ancestral info includes:
  * ancestralComponentStateSequences: Int (node) -> VectorPairIntInt (component, state)
  * ancestralStateSequences:          Int (node) -> EVector Int (state)
  * ancestralStateSequences':         Int (node) -> EVector Int (state) with gaps
  * ancestralLetterSequences:         Int (node) -> EVector Int (letter)
  * sequenceFromTree ancestralLetterSequences: [(Text,EVector Int)]
  * ancestralSequences:               Aligned (CharacterData alphabet _)
-}

{- For VariableA.hs, ancestral info includes:
  * ancestralComponentStateSequences: Int (node) -> VectorPairIntInt (component, state)
  * ancestral_sequences:              Int (node) -> EVector Int (state)
  * alignedSequences alignment _:     Int (node) -> EVector Int (state)
  * stateToLetters smap _:            Int (node) -> EVector Int (letter)
  * sequenceFromTree _:               [(Text, EVector Int)]
  * ancestralSequences:               Aligned (CharacterData alphabet _)
-}
