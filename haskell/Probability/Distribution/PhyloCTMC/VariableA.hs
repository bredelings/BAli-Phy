module Probability.Distribution.PhyloCTMC.VariableA
    (module Probability.Distribution.PhyloCTMC.Properties,
     module Probability.Distribution.PhyloCTMC.PhyloCTMC
    )
where

import Probability.Distribution.PhyloCTMC.Properties
import Probability.Distribution.PhyloCTMC.PhyloCTMC
import Probability.Random
import Tree
import SModel
import SModel.Likelihood.VariableA
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

annotated_subst_like_on_tree tree alignment smodel sequenceData = do
  let rtree = setRoot subst_root (makeRooted tree)
      subst_root = modifiable (head $ internalNodes tree ++ leafNodes tree)

  let n_nodes = numNodes rtree
      as = pairwise_alignments alignment
      maybeNodeSequences = labelToNodeMap rtree (getSequences sequenceData)
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel_on_tree
      smodel_on_tree = SModelOnTree rtree smodel
      transitionPs = transitionPsMap smodel_on_tree
      f = weighted_frequency_matrix smodel_on_tree
      fs = getNodesSet rtree & IntMap.fromSet (\_ -> f)
      cls = cached_conditional_likelihoods rtree nodeCLVs as transitionPs f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peel_likelihood rtree nodeCLVs cls as f subst_root

      ancestralComponentStateSequences = sample_ancestral_sequences rtree subst_root nodeCLVs as transitionPs f cls

      ancestral_sequences = extractStates <$> ancestralComponentStateSequences

      ancestralSequences = Aligned $ CharacterData alphabet (sequencesFromTree rtree (statesToLetters smap <$> alignedSequences alignment ancestral_sequences))

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = PhyloCTMCProperties subst_root transitionPs cls ancestralSequences likelihood fs smap nodeCLVs alphabet (SModel.nStates smodel_on_tree) (SModel.nBaseModels smodel_on_tree)

  return ([likelihood], prop)

instance Dist (PhyloCTMC t (AlignmentOnTree t) s EquilibriumReversible) where
    type Result (PhyloCTMC t (AlignmentOnTree t) s EquilibriumReversible) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType (Rooted t) ~ Text, HasRoot (Rooted t), HasBranchLengths (Rooted t), RateModel s, IsTree t, SimpleSModel (Rooted t) s) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t) s EquilibriumReversible) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t) s EquilibriumReversible) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree alignment smodel scale) = annotated_subst_like_on_tree tree alignment (rescale scale smodel)

-- getSequencesFromTree :: IsGraph t, LabelType t ~ Text => t -> IntMap Sequence ->

-- Uh... how do we get from a [Sequence] and an AlignmentOnTree to a FASTA file?
-- Can we have an IsAlignment class that allows us to get a FASTA from it?
--  + alignmentLength can be implemented for both AlignmentMatrix and AlignmentOnTree
--  + could we change AlignmentOnTree to be data AlignmentOnTree t = OneSequence { tree :: t, length :: Int } | ManySequences { tree :: t, paiwise_alignments :: IntMap PairwiseAlignment} ?
--    one issue is that we can check EITHER the tree OR the alignment constructor to determin of there's only one sequence.
--    We could to AlignmentOnTree { tree :: t, singleLength :: Maybe Int, pairwise_alignments :: IntMap PairwiseAlignment }
--    In that case, if there's a single length, we still have an (empty) IntMap for the pairwise alignments.

foreign import bpcall "Likelihood:" simulateRootSequence :: Int -> Matrix Double -> IO VectorPairIntInt
foreign import bpcall "Likelihood:" simulateSequenceFrom :: VectorPairIntInt -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> IO VectorPairIntInt

sampleComponentStates rtree alignment smodel =  do
  let smodel_on_tree = SModelOnTree rtree smodel
      as = pairwise_alignments alignment
      ps = transitionPsMap smodel_on_tree
      f = (weighted_frequency_matrix smodel_on_tree)

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence (sequenceLength alignment node) f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateSequenceFrom (stateSequences IntMap.! parent) (as IntMap.! b) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences


instance (HasAlphabet s, IsTree t, HasRoot (Rooted t), LabelType (Rooted t) ~ Text, HasBranchLengths (Rooted t), RateModel s, SimpleSModel (Rooted t) s) => IOSampleable (PhyloCTMC t (AlignmentOnTree t) s EquilibriumReversible) where
    sampleIO (PhyloCTMC tree alignment rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          rtree = makeRooted tree
          smodel = rescale scale rawSmodel
          smap = stateLetters (SModelOnTree rtree smodel)

      stateSequences <- sampleComponentStates rtree alignment smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled rtree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot (Rooted t), LabelType (Rooted t) ~ Text, HasBranchLengths (Rooted t), RateModel s, SimpleSModel (Rooted t) s) => Sampleable (PhyloCTMC t (AlignmentOnTree t) s EquilibriumReversible) where
    sample dist = RanDistribution2 dist do_nothing


-----------
annotatedSubstLikeOnTreeEqNonRev tree alignment smodel sequenceData = do
  let subst_root = modifiable (head $ internalNodes tree ++ leafNodes tree)

  let n_nodes = numNodes tree
      as = pairwise_alignments alignment
      maybeNodeSequences = labelToNodeMap tree (getSequences sequenceData)
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel_on_tree
      smodel_on_tree = SModelOnTree tree smodel
      transitionPs = transitionPsMap smodel_on_tree
      f = weighted_frequency_matrix smodel_on_tree
      fs = getNodesSet tree & IntMap.fromSet (\_ -> f)
      cls = cachedConditionalLikelihoodsEqNonRev tree nodeCLVs as transitionPs f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peelLikelihoodEqNonRev tree nodeCLVs cls as f subst_root

      ancestralComponentStateSequences = sample_ancestral_sequences tree subst_root nodeCLVs as transitionPs f cls

      ancestral_sequences = extractStates <$> ancestralComponentStateSequences

      ancestralSequences = Aligned $ CharacterData alphabet (sequencesFromTree tree (statesToLetters smap <$> alignedSequences alignment ancestral_sequences))

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = PhyloCTMCProperties subst_root transitionPs cls ancestralSequences likelihood fs smap nodeCLVs alphabet (SModel.nStates smodel_on_tree) (SModel.nBaseModels smodel_on_tree)

  return ([likelihood], prop)

instance t ~ t2 => Dist (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumNonReversible) where
    type Result (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumNonReversible) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s, t ~ t2) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumNonReversible) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumNonReversible) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree alignment smodel scale) = annotatedSubstLikeOnTreeEqNonRev tree alignment (rescale scale smodel)

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => IOSampleable (PhyloCTMC t (AlignmentOnTree t) s EquilibriumNonReversible) where
    sampleIO (PhyloCTMC tree alignment rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          smodel = rescale scale rawSmodel
          smap = stateLetters (SModelOnTree tree smodel)

      stateSequences <- sampleComponentStates tree alignment smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => Sampleable (PhyloCTMC t (AlignmentOnTree t) s EquilibriumNonReversible) where
    sample dist = RanDistribution2 dist do_nothing

---------------------------------------------
annotatedSubstLikeOnTreeNonEq tree alignment smodel sequenceData = do
  let subst_root = modifiable (head $ internalNodes tree ++ leafNodes tree)

  let n_nodes = numNodes tree
      as = pairwise_alignments alignment
      maybeNodeSequences = labelToNodeMap tree (getSequences sequenceData)
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel_on_tree
      smodel_on_tree = SModelOnTree tree smodel
      transitionPs = transitionPsMap smodel_on_tree
      f = weighted_frequency_matrix smodel_on_tree
      fs = frequenciesOnTree tree f transitionPs
      cls = cachedConditionalLikelihoodsNonEq tree nodeCLVs as transitionPs f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peelLikelihoodNonEq tree nodeCLVs cls as f subst_root

      ancestralComponentStateSequences = sample_ancestral_sequences tree subst_root nodeCLVs as transitionPs f cls

      ancestral_sequences = extractStates <$> ancestralComponentStateSequences

      ancestralSequences = Aligned $ CharacterData alphabet (sequencesFromTree tree (statesToLetters smap <$> alignedSequences alignment ancestral_sequences))

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = PhyloCTMCProperties subst_root transitionPs cls ancestralSequences likelihood fs smap nodeCLVs alphabet (SModel.nStates smodel_on_tree) (SModel.nBaseModels smodel_on_tree)

  return ([likelihood], prop)

instance t ~ t2 => Dist (PhyloCTMC t (AlignmentOnTree t2) s NonEquilibrium) where
    type Result (PhyloCTMC t (AlignmentOnTree t2) s NonEquilibrium) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s, t ~ t2) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t2) s NonEquilibrium) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t2) s NonEquilibrium) = PhyloCTMCProperties
    annotated_densities (PhyloCTMC tree alignment smodel scale) = annotatedSubstLikeOnTreeNonEq tree alignment (rescale scale smodel)

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => IOSampleable (PhyloCTMC t (AlignmentOnTree t) s NonEquilibrium) where
    sampleIO (PhyloCTMC tree alignment rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          smodel = rescale scale rawSmodel
          smap = stateLetters (SModelOnTree tree smodel)

      stateSequences <- sampleComponentStates tree alignment smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => Sampleable (PhyloCTMC t (AlignmentOnTree t) s NonEquilibrium) where
    sample dist = RanDistribution2 dist do_nothing


