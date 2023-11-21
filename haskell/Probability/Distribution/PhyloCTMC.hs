module Probability.Distribution.PhyloCTMC where

import Probability.Random
import Tree
import SModel
import Bio.Sequence -- for sequence_to_indices
import Bio.Alignment
import Bio.Alphabet  -- for type Alphabet
import Data.Array
import Data.Matrix
import Data.Foldable
import Foreign.Maybe
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Maybe (fromJust)

import Control.Monad.Fix -- for rec
import Probability.Distribution.List -- for independent

-- 0 - subst_root
-- 1 - transition_ps
-- 2 - CLVs
-- 3 - ?ancestral states -- used through haskell, not parameters.cc
-- 4 - likelihood
-- 5 - weightedfrequencymatrix
-- 6 - state -> letter
-- 7 - sequences as EVector Int  -- only to get CLVs for single node when aligning 2 sequences.
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
      prop_get_weighted_frequency_matrix :: Matrix Double,
      prop_smap :: EVector Int,
      prop_leaf_sequences :: IntMap (EVector Int),
      prop_alphabet :: Alphabet,
      prop_n_states :: Int,
      prop_n_base_models :: Int,
      prop_n_muts :: Int -- This shouldn't be here.
    }

{- NOTE: Sampling from phyloCTMC and phyloCTMCFixedA.

The idea is to sample an UnalignedCharacterData object from phyloCTMC, and an AlignedCharacterData object from phyloCTMCFixedA.

We could make a function alignCharacterData :: AlignmentOnTree -> UnalignedCharacterData -> AlignedCharacterData

For sampling from phyloCTMCFixedA, we might want two versions:
1. given a length l, sample assuming no indels or wildcards
2. given a alignment with gaps and wildcards, sample assuming no indels and then overwrite gaps and wildcards.
   PROBLEM: what about ambiguous characters?
-}

annotated_subst_like_on_tree tree alignment smodel scale sequenceData = do
  let subst_root = modifiable (head $ internal_nodes tree ++ leaf_nodes tree)

  let n_nodes = numNodes tree
      as = pairwise_alignments alignment
      maybeNodeSequences = labelToNodeMap tree (getSequences sequenceData)
      node_sequences = fromMaybe (error "No Label") <$> maybeNodeSequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel scale
      transition_ps = transition_ps_map smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods
              tree
              maybeNodeSequences
              as
              alphabet
              transition_ps
              f
              smap
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peel_likelihood tree maybeNodeSequences cls as alphabet (weighted_frequency_matrix smodel) smap subst_root

      ancestralComponentStateSequences = sample_ancestral_sequences tree subst_root maybeNodeSequences as alphabet transition_ps f cls smap

      ancestral_sequences = extractStates <$> ancestralComponentStateSequences

      ancestralSequences = Aligned $ CharacterData alphabet (sequencesFromTree tree (statesToLetters smap <$> alignedSequences alignment ancestral_sequences))

      n_muts = parsimony tree maybeNodeSequences as alphabet (unitCostMatrix alphabet)

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = (PhyloCTMCProperties subst_root transition_ps cls ancestralSequences likelihood f smap node_sequences alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel) n_muts)

  return ([likelihood], prop)

data PhyloCTMC t a s = PhyloCTMC t a s Double

instance Dist (PhyloCTMC t (AlignmentOnTree t) s) where
    type Result (PhyloCTMC t (AlignmentOnTree t) s) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"

-- TODO: make this work on forests!                  -
instance (HasLabels t, HasBranchLengths t, IsTree t, SimpleSModel s) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t) s) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t) s) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree alignment smodel scale) = annotated_subst_like_on_tree tree alignment smodel scale

phyloCTMC tree alignment smodel scale = PhyloCTMC tree alignment smodel scale

-- getSequencesFromTree :: HasLabels t => t -> IntMap Sequence ->

-- Uh... how do we get from a [Sequence] and an AlignmentOnTree to a FASTA file?
-- Can we have an IsAlignment class that allows us to get a FASTA from it?
--  + alignmentLength can be implemented for both AlignmentMatrix and AlignmentOnTree
--  + could we change AlignmentOnTree to be data AlignmentOnTree t = OneSequence { tree :: t, length :: Int } | ManySequences { tree :: t, paiwise_alignments :: IntMap PairwiseAlignment} ?
--    one issue is that we can check EITHER the tree OR the alignment constructor to determin of there's only one sequence.
--    We could to AlignmentOnTree { tree :: t, singleLength :: Maybe Int, pairwise_alignments :: IntMap PairwiseAlignment }
--    In that case, if there's a single length, we still have an (empty) IntMap for the pairwise alignments.

foreign import bpcall "Likelihood:" simulateRootSequence :: Int -> Matrix Double -> IO VectorPairIntInt
foreign import bpcall "Likelihood:" simulateSequenceFrom :: VectorPairIntInt -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> IO VectorPairIntInt

sampleComponentStates rtree alignment smodel scale =  do
  let as = pairwise_alignments alignment
      ps = transition_ps_map (SingleBranchLengthModel rtree smodel scale)
      f = (weighted_frequency_matrix smodel)

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence (sequenceLength alignment node) f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateSequenceFrom (stateSequences IntMap.! parent) (as IntMap.! b) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences


instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths (Rooted t), SimpleSModel s) => IOSampleable (PhyloCTMC t (AlignmentOnTree t) s) where
    sampleIO (PhyloCTMC tree alignment smodel scale) = do
      let alphabet = getAlphabet smodel
          smap = stateLetters smodel

      stateSequences <- sampleComponentStates (makeRooted tree) alignment smodel scale

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths t, HasBranchLengths (Rooted t), SimpleSModel s) => Sampleable (PhyloCTMC t (AlignmentOnTree t) s) where
    sample dist = RanDistribution2 dist do_nothing


----------------------------------------

{-
ok, so how do we pass IntMaps to C++ functions?
well, we could turn each IntMap into an EIntMap
for alignments, we could also use an ordering of the sequences to ensure that the leaves are written first.
   -}
annotated_subst_likelihood_fixed_A tree length smodel scale sequenceData = do
  let subst_root = modifiable (head $ internal_nodes tree ++ leaf_nodes tree)

  let (isequences, column_counts, mapping) = compress_alignment $ getSequences sequenceData

      maybeNodeISequences = labelToNodeMap tree isequences
      maybeNodeSeqsBits = ((\seq -> (strip_gaps seq, bitmask_from_sequence seq)) <$>) <$> maybeNodeISequences

      node_sequences0 :: IntMap (Maybe (EVector Int))
      node_sequences0 = labelToNodeMap tree $ getSequences sequenceData

      n_nodes = numNodes tree
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel scale
      transition_ps = transition_ps_map smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods_SEV
              tree
              maybeNodeSeqsBits
              alphabet
              transition_ps
              smap
      likelihood = peel_likelihood_SEV maybeNodeSeqsBits tree cls f alphabet smap subst_root column_counts

--    This also needs the map from columns to compressed columns:
      ancestralSequences = case n_nodes of
                              1 -> sequenceData
                              2 -> sequenceData
                              _ -> let ancestralComponentStateSequences :: IntMap VectorPairIntInt
                                       ancestralComponentStateSequences = sample_ancestral_sequences_SEV
                                         tree
                                         subst_root
                                         maybeNodeSeqsBits
                                         alphabet
                                         transition_ps
                                         f
                                         cls
                                         smap
                                         mapping
                                       ancestralStateSequences :: IntMap (EVector Int)
                                       ancestralStateSequences = extractStates <$> ancestralComponentStateSequences
                                       ancestralStateSequences' = minimally_connect_characters
                                                                        node_sequences0
                                                                        tree
                                                                        ancestralStateSequences
                                       ancestralLetterSequences = statesToLetters smap <$> ancestralStateSequences'
                                   in Aligned (CharacterData alphabet $ sequencesFromTree tree ancestralLetterSequences)

      n_muts = parsimony_fixed_A tree maybeNodeSeqsBits alphabet (unitCostMatrix alphabet) column_counts

  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  let prop = (PhyloCTMCProperties subst_root transition_ps cls ancestralSequences likelihood f smap undefined alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel) n_muts)

  return ([likelihood], prop)

instance Dist (PhyloCTMC t Int s) where
    type Result (PhyloCTMC t Int s) = AlignedCharacterData
    dist_name _ = "PhyloCTMCFixedA"

-- TODO: make this work on forests!                  -
instance (HasLabels t, HasBranchLengths t, IsTree t, SimpleSModel s) => HasAnnotatedPdf (PhyloCTMC t Int s) where
    type DistProperties (PhyloCTMC t Int s) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree length smodel scale) = annotated_subst_likelihood_fixed_A tree length smodel scale

foreign import bpcall "Likelihood:" simulateFixedSequenceFrom :: VectorPairIntInt -> EVector (Matrix Double) -> Matrix Double -> IO VectorPairIntInt

sampleComponentStatesFixed rtree rootLength smodel scale =  do
  let ps = transition_ps_map (SingleBranchLengthModel rtree smodel scale)
      f = (weighted_frequency_matrix smodel)

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence rootLength f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateFixedSequenceFrom (stateSequences IntMap.! parent) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences


instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths (Rooted t), SimpleSModel s) => IOSampleable (PhyloCTMC t Int s) where
    sampleIO (PhyloCTMC tree rootLength smodel scale) = do
      let alphabet = getAlphabet smodel
          smap = stateLetters smodel

      stateSequences <- sampleComponentStatesFixed (makeRooted tree) rootLength smodel scale

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths t, HasBranchLengths (Rooted t), SimpleSModel s) => Sampleable (PhyloCTMC t Int s) where
    sample dist = RanDistribution2 dist do_nothing


