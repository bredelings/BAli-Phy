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

-- Some of these things could be accessed through the distribution arguments:
-- 6. state -> letter
-- 7. sequences as EVector Int
-- 8. alphabet
-- 9. n_states
-- 10. n_base_models


data PhyloCTMCPropertiesFixedA = PhyloCTMCPropertiesFixedA {
      prop_fixed_a_subst_root :: Int,
      prop_fixed_a_transition_ps :: IntMap (EVector (Matrix Double)),
      prop_fixed_a_cond_likes :: IntMap CondLikes,
      prop_fixed_a_anc_seqs :: AlignedCharacterData,
      prop_fixed_a_likelihood :: LogDouble,
      prop_fixed_a_get_weightedFrequencyMatrix :: IntMap (Matrix Double),   -- only variable A
      prop_fixed_a_smap :: EVector Int,
      prop_fixed_a_nodeCLVs :: IntMap (Maybe CondLikes),                    -- only variable A
      prop_fixed_a_alphabet :: Alphabet,
      prop_fixed_a_n_states :: Int,
      prop_fixed_a_n_base_models :: Int
    }


data PhyloCTMCPropertiesVariableA = PhyloCTMCPropertiesVariableA {
      prop_variable_a_subst_root :: Int,
      prop_variable_a_transition_ps :: IntMap (EVector (Matrix Double)),
      prop_variable_a_cond_likes :: IntMap CondLikes,
      prop_variable_a_anc_seqs :: AlignedCharacterData,
      prop_variable_a_likelihood :: LogDouble,
      prop_variable_a_get_weightedFrequencyMatrix :: IntMap (Matrix Double),   -- only variable A
      prop_variable_a_smap :: EVector Int,
      prop_variable_a_nodeCLVs :: IntMap (Maybe CondLikes),                    -- only variable A
      prop_variable_a_alphabet :: Alphabet,
      prop_variable_a_n_states :: Int,
      prop_variable_a_n_base_models :: Int
    }


class PhyloCTMCProperties a where
      prop_subst_root :: a -> Int
      prop_transition_ps :: a -> IntMap (EVector (Matrix Double))
      prop_cond_likes :: a -> IntMap CondLikes
      prop_anc_seqs :: a -> AlignedCharacterData
      prop_likelihood :: a -> LogDouble
      prop_get_weightedFrequencyMatrix :: a -> IntMap (Matrix Double)    -- only variable A
      prop_smap :: a -> EVector Int
      prop_nodeCLVs :: a -> IntMap (Maybe CondLikes)                    -- only variable A
      prop_alphabet :: a -> Alphabet
      prop_n_states :: a -> Int
      prop_n_base_models :: a -> Int


instance PhyloCTMCProperties PhyloCTMCPropertiesFixedA where
    prop_subst_root = prop_fixed_a_subst_root
    prop_transition_ps = prop_fixed_a_transition_ps
    prop_cond_likes = prop_fixed_a_cond_likes
    prop_anc_seqs = prop_fixed_a_anc_seqs
    prop_likelihood = prop_fixed_a_likelihood
    prop_get_weightedFrequencyMatrix = prop_fixed_a_get_weightedFrequencyMatrix
    prop_smap = prop_fixed_a_smap 
    prop_nodeCLVs = prop_fixed_a_nodeCLVs
    prop_alphabet = prop_fixed_a_alphabet
    prop_n_states = prop_fixed_a_n_states
    prop_n_base_models = prop_fixed_a_n_base_models


instance PhyloCTMCProperties PhyloCTMCPropertiesVariableA where
    prop_subst_root = prop_variable_a_subst_root
    prop_transition_ps = prop_variable_a_transition_ps
    prop_cond_likes = prop_variable_a_cond_likes
    prop_anc_seqs = prop_variable_a_anc_seqs
    prop_likelihood = prop_variable_a_likelihood
    prop_get_weightedFrequencyMatrix = prop_variable_a_get_weightedFrequencyMatrix
    prop_smap = prop_variable_a_smap 
    prop_nodeCLVs = prop_variable_a_nodeCLVs
    prop_alphabet = prop_variable_a_alphabet
    prop_n_states = prop_variable_a_n_states
    prop_n_base_models = prop_variable_a_n_base_models
                            

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

{- Can we separate out the alignment and the character properties?
   The alignment could be converted into an Int (node) -> BitSet
   * I think we could modify minimally_connect_characters uncompressedNodeSequences to do this.
     - Should we have a type for [(Text, Bitset)]?
     - Should we be able to apply Aligned or Unaligned to this, even though its not CharacterData?
     - Can we use such a thing to MASK the ancestralComponentStateSequences?
   * Can we 

Then we could store site properties as Int (node) -> EVector Double
 -}

{- What does it mean to identify the ancestral rate for an ancestral character?
   The character at a node that is ancestral to a group of observed sequences could
      be a collection of characters.
   We could average across such characters...
 -}
