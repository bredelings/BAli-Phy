module Probability.Distribution.PhyloCTMC.FixedA
    (module Probability.Distribution.PhyloCTMC.Properties,
     module Probability.Distribution.PhyloCTMC.PhyloCTMC
    )

where

import Probability.Distribution.PhyloCTMC.Properties
import Probability.Distribution.PhyloCTMC.PhyloCTMC
import Probability.Random
import Tree
import SModel
import SModel.Likelihood.FixedA
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
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits

      uncompressedNodeSequences :: IntMap (Maybe (EVector Int))
      uncompressedNodeSequences = labelToNodeMap tree $ getSequences sequenceData

      n_nodes = numNodes tree
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel scale
      transition_ps = transition_ps_map smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods tree nodeCLVs transition_ps
      likelihood = peel_likelihood nodeCLVs tree cls f alphabet smap subst_root column_counts

--    This also needs the map from columns to compressed columns:
      ancestralSequences = let ancestralComponentStateSequences :: IntMap VectorPairIntInt
                               ancestralComponentStateSequences = sample_ancestral_sequences
                                                                     tree
                                                                     subst_root
                                                                     nodeCLVs
                                                                     alphabet
                                                                     transition_ps
                                                                     f
                                                                     cls
                                                                     smap
                                                                     mapping
                               ancestralStateSequences :: IntMap (EVector Int)
                               ancestralStateSequences = extractStates <$> ancestralComponentStateSequences
                               ancestralStateSequences' = minimally_connect_characters
                                                                     uncompressedNodeSequences
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

instance Dist (PhyloCTMC t Int s Reversible) where
    type Result (PhyloCTMC t Int s Reversible) = AlignedCharacterData
    dist_name _ = "PhyloCTMCFixedA"

-- TODO: make this work on forests!                  -
instance (HasLabels t, HasBranchLengths t, IsTree t, SimpleSModel s) => HasAnnotatedPdf (PhyloCTMC t Int s Reversible) where
    type DistProperties (PhyloCTMC t Int s Reversible) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree length smodel scale) = annotated_subst_likelihood_fixed_A tree length smodel scale

-- This is imported twice, which is ugly.
foreign import bpcall "Likelihood:" simulateRootSequence :: Int -> Matrix Double -> IO VectorPairIntInt
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


instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths (Rooted t), SimpleSModel s) => IOSampleable (PhyloCTMC t Int s Reversible) where
    sampleIO (PhyloCTMC tree rootLength smodel scale) = do
      let alphabet = getAlphabet smodel
          smap = stateLetters smodel

      stateSequences <- sampleComponentStatesFixed (makeRooted tree) rootLength smodel scale

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (IsTree t, HasRoot (Rooted t), HasLabels t, HasBranchLengths t, HasBranchLengths (Rooted t), SimpleSModel s) => Sampleable (PhyloCTMC t Int s Reversible) where
    sample dist = RanDistribution2 dist do_nothing


--------------
{-
ok, so how do we pass IntMaps to C++ functions?
well, we could turn each IntMap into an EIntMap
for alignments, we could also use an ordering of the sequences to ensure that the leaves are written first.
   -}
annotatedSubstLikelihoodFixedANonRev tree length smodel scale sequenceData = do
  let subst_root = modifiable (head $ internal_nodes tree ++ leaf_nodes tree)

  let (isequences, column_counts, mapping) = compress_alignment $ getSequences sequenceData

      maybeNodeISequences = labelToNodeMap tree isequences
      maybeNodeSeqsBits = ((\seq -> (strip_gaps seq, bitmask_from_sequence seq)) <$>) <$> maybeNodeISequences
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits

      uncompressedNodeSequences :: IntMap (Maybe (EVector Int))
      uncompressedNodeSequences = labelToNodeMap tree $ getSequences sequenceData

      n_nodes = numNodes tree
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel scale
      transition_ps = transition_ps_map smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cachedConditionalLikelihoodsNonRev tree nodeCLVs transition_ps f
      likelihood = peelLikelihoodNonRev nodeCLVs tree cls f alphabet smap subst_root column_counts

--    This also needs the map from columns to compressed columns:
      ancestralSequences = let ancestralComponentStateSequences :: IntMap VectorPairIntInt
                               ancestralComponentStateSequences = sample_ancestral_sequences
                                                                     tree
                                                                     subst_root
                                                                     nodeCLVs
                                                                     alphabet
                                                                     transition_ps
                                                                     f
                                                                     cls
                                                                     smap
                                                                     mapping
                               ancestralStateSequences :: IntMap (EVector Int)
                               ancestralStateSequences = extractStates <$> ancestralComponentStateSequences
                               ancestralStateSequences' = minimally_connect_characters
                                                                     uncompressedNodeSequences
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

instance Dist (PhyloCTMC t Int s NonReversible) where
    type Result (PhyloCTMC t Int s NonReversible) = AlignedCharacterData
    dist_name _ = "PhyloCTMCFixedA"

-- TODO: make this work on forests!                  -
instance (HasLabels t, HasRoot t, HasBranchLengths t, IsTree t, SimpleSModel s) => HasAnnotatedPdf (PhyloCTMC t Int s NonReversible) where
    type DistProperties (PhyloCTMC t Int s NonReversible) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree length smodel scale) = annotatedSubstLikelihoodFixedANonRev tree length smodel scale


sampleComponentStatesFixedNonRev rtree rootLength smodel scale =  do
  let ps = transition_ps_map (SingleBranchLengthModel rtree smodel scale)
      f = (weighted_frequency_matrix smodel)

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence rootLength f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateFixedSequenceFrom (stateSequences IntMap.! parent) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences


-- Should hasRoots t imply HasRoots (Rooted t)?
-- Where is (Rooted t) coming up?  Can we remove it?

instance (IsTree t, HasRoot t, HasLabels t, HasBranchLengths t, SimpleSModel s) => IOSampleable (PhyloCTMC t Int s NonReversible) where
    sampleIO (PhyloCTMC tree rootLength smodel scale) = do
      let alphabet = getAlphabet smodel
          smap = stateLetters smodel

      stateSequences <- sampleComponentStatesFixedNonRev tree rootLength smodel scale

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (IsTree t, HasRoot t, HasLabels t, HasBranchLengths t, HasBranchLengths t, SimpleSModel s) => Sampleable (PhyloCTMC t Int s NonReversible) where
    sample dist = RanDistribution2 dist do_nothing


