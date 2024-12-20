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

sampleAncestralAlignment uncompressedNodeSequences tree substRoot nodeCLVs alphabet transitionPs f cls smap mapping =
--    This also needs the map from columns to compressed columns:
      let ancestralComponentStateSequences :: IntMap VectorPairIntInt
          ancestralComponentStateSequences = sample_ancestral_sequences tree substRoot nodeCLVs alphabet transitionPs f cls smap mapping

          ancestralStateSequences :: IntMap (EVector Int)
          ancestralStateSequences = extractStates <$> ancestralComponentStateSequences
          ancestralStateSequences' = minimally_connect_characters uncompressedNodeSequences tree ancestralStateSequences

          ancestralLetterSequences = statesToLetters smap <$> ancestralStateSequences'

      in Aligned (CharacterData alphabet $ sequencesFromTree tree ancestralLetterSequences)


{-
ok, so how do we pass IntMaps to C++ functions?
well, we could turn each IntMap into an EIntMap
for alignments, we could also use an ordering of the sequences to ensure that the leaves are written first.
   -}
annotated_subst_likelihood_fixed_A tree length smodel sequenceData = do
  let rtree = setRoot substRoot (makeRooted tree)
      substRoot = modifiable (head $ internalNodes rtree ++ leafNodes rtree)

  let (isequences, column_counts, mapping) = compress_alignment $ getSequences sequenceData

      maybeNodeISequences = labelToNodeMap rtree isequences
      maybeNodeSeqsBits = ((\seq -> (strip_gaps seq, bitmask_from_sequence seq)) <$>) <$> maybeNodeISequences
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits

      uncompressedNodeSequences :: IntMap (Maybe (EVector Int))
      uncompressedNodeSequences = labelToNodeMap rtree $ getSequences sequenceData

      n_nodes = numNodes rtree
      alphabet = getAlphabet smodel
      smap   = stateLetters smodelOnTree
      smodelOnTree = SModelOnTree rtree smodel
      transitionPs = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree
      cls = cached_conditional_likelihoods rtree nodeCLVs transitionPs
      likelihood = peel_likelihood nodeCLVs rtree cls f alphabet smap substRoot column_counts

      ancestralSequences = sampleAncestralAlignment uncompressedNodeSequences rtree substRoot nodeCLVs alphabet transitionPs f cls smap mapping

  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  let prop = PhyloCTMCProperties substRoot transitionPs cls ancestralSequences likelihood undefined smap undefined alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree)

  return ([likelihood], prop)

instance Dist (PhyloCTMC t Int s EquilibriumReversible) where
    type Result (PhyloCTMC t Int s EquilibriumReversible) = AlignedCharacterData
    dist_name _ = "PhyloCTMCFixedA"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType (Rooted t) ~ Text, HasRoot (Rooted t), HasBranchLengths (Rooted t), RateModel s, IsTree t, SimpleSModel (Rooted t) s) => HasAnnotatedPdf (PhyloCTMC t Int s EquilibriumReversible) where
    type DistProperties (PhyloCTMC t Int s EquilibriumReversible) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree length smodel scale) = annotated_subst_likelihood_fixed_A tree length (rescale scale smodel)

-- This is imported twice, which is ugly.
foreign import bpcall "Likelihood:" simulateRootSequence :: Int -> Matrix Double -> IO VectorPairIntInt
foreign import bpcall "Likelihood:" simulateFixedSequenceFrom :: VectorPairIntInt -> EVector (Matrix Double) -> Matrix Double -> IO VectorPairIntInt

sampleComponentStatesFixed rtree rootLength smodel =  do
  let smodelOnTree = SModelOnTree rtree smodel
      ps = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence rootLength f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateFixedSequenceFrom (stateSequences IntMap.! parent) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences


instance (HasAlphabet s, IsTree t, HasRoot (Rooted t), LabelType (Rooted t) ~ Text, HasBranchLengths (Rooted t), RateModel s, SimpleSModel (Rooted t) s) => IOSampleable (PhyloCTMC t Int s EquilibriumReversible) where
    sampleIO (PhyloCTMC tree rootLength rawSmodel scale) = do
      let rtree = makeRooted tree
          alphabet = getAlphabet smodel
          smodel = rescale scale rawSmodel
          smap = stateLetters (SModelOnTree rtree smodel)

      stateSequences <- sampleComponentStatesFixed rtree rootLength smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled rtree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot (Rooted t), LabelType (Rooted t) ~ Text, HasBranchLengths (Rooted t), HasBranchLengths (Rooted t), RateModel s, SimpleSModel (Rooted t) s) => Sampleable (PhyloCTMC t Int s EquilibriumReversible) where
    sample dist = RanDistribution2 dist do_nothing


--------------
{-
ok, so how do we pass IntMaps to C++ functions?
well, we could turn each IntMap into an EIntMap
for alignments, we could also use an ordering of the sequences to ensure that the leaves are written first.
   -}
annotatedSubstLikelihoodFixedANonRev tree length smodel sequenceData = do
  let substRoot = modifiable (head $ internalNodes tree ++ leafNodes tree)

  let (isequences, column_counts, mapping) = compress_alignment $ getSequences sequenceData

      maybeNodeISequences = labelToNodeMap tree isequences
      maybeNodeSeqsBits = ((\seq -> (strip_gaps seq, bitmask_from_sequence seq)) <$>) <$> maybeNodeISequences
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits

      uncompressedNodeSequences :: IntMap (Maybe (EVector Int))
      uncompressedNodeSequences = labelToNodeMap tree $ getSequences sequenceData

      n_nodes = numNodes tree
      alphabet = getAlphabet smodel
      smap   = stateLetters smodelOnTree
      smodelOnTree = SModelOnTree tree smodel
      transitionPs = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree
      cls = cachedConditionalLikelihoodsNonRev tree nodeCLVs transitionPs f
      likelihood = peelLikelihoodNonRev nodeCLVs tree cls f alphabet smap substRoot column_counts

      ancestralSequences = sampleAncestralAlignment uncompressedNodeSequences tree substRoot nodeCLVs alphabet transitionPs f cls smap mapping

  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  let prop = PhyloCTMCProperties substRoot transitionPs cls ancestralSequences likelihood undefined smap undefined alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree)

  return ([likelihood], prop)

instance Dist (PhyloCTMC t Int s EquilibriumNonReversible) where
    type Result (PhyloCTMC t Int s EquilibriumNonReversible) = AlignedCharacterData
    dist_name _ = "PhyloCTMCFixedA"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s) => HasAnnotatedPdf (PhyloCTMC t Int s EquilibriumNonReversible) where
    type DistProperties (PhyloCTMC t Int s EquilibriumNonReversible) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree length smodel scale) = annotatedSubstLikelihoodFixedANonRev tree length (rescale scale smodel)

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => IOSampleable (PhyloCTMC t Int s EquilibriumNonReversible) where
    sampleIO (PhyloCTMC tree rootLength rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          smodel = rescale scale rawSmodel
          smap = stateLetters (SModelOnTree tree smodel)

      stateSequences <- sampleComponentStatesFixed tree rootLength smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => Sampleable (PhyloCTMC t Int s EquilibriumNonReversible) where
    sample dist = RanDistribution2 dist do_nothing


--------------

instance Dist (PhyloCTMC t Int s NonEquilibrium) where
    type Result (PhyloCTMC t Int s NonEquilibrium) = AlignedCharacterData
    dist_name _ = "PhyloCTMCFixedA"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s) => HasAnnotatedPdf (PhyloCTMC t Int s NonEquilibrium) where
    type DistProperties (PhyloCTMC t Int s NonEquilibrium) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree length smodel scale) = annotatedSubstLikelihoodFixedANonRev tree length (rescale scale smodel)

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => IOSampleable (PhyloCTMC t Int s NonEquilibrium) where
    sampleIO (PhyloCTMC tree rootLength rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          smodel = rescale scale rawSmodel
          smap = stateLetters (SModelOnTree tree smodel)

      stateSequences <- sampleComponentStatesFixed tree rootLength smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => Sampleable (PhyloCTMC t Int s NonEquilibrium) where
    sample dist = RanDistribution2 dist do_nothing


