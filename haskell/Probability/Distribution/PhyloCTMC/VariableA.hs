module Probability.Distribution.PhyloCTMC.VariableA
    (module Probability.Distribution.PhyloCTMC.Properties,
     module Probability.Distribution.PhyloCTMC.PhyloCTMC
    )
where

import Probability.Distribution.PhyloCTMC.VariableA.Properties
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
import SModel.Likelihood.CLV (CondLikes)

import Control.Monad.Fix -- for rec

{-
 - rtree: translate from nodes to labels
 - alignment: translates from sequences to aligned sequences
 - smap: translates from states to letters
 - alphabet: describes the letters
 -}

annotated_subst_like_on_tree tree alignment smodel sequenceData = do
  let rtree = setRoot substRoot (makeRooted tree)
      substRoot = modifiable (head $ internalNodes tree ++ leafNodes tree)

  let n_nodes = numNodes rtree
      as = pairwise_alignments alignment
      maybeNodeSequences = labelToNodeMap rtree (getSequences sequenceData)
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodelOnTree
      smodelOnTree = SModelOnTree rtree smodel
      transitionPs = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree
      fs = getNodesSet rtree & IntMap.fromSet (\_ -> f)
      cls = cached_conditional_likelihoods rtree nodeCLVs as transitionPs f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peel_likelihood rtree nodeCLVs cls as f substRoot

      ancestralComponentStates = sample_ancestral_sequences rtree substRoot nodeCLVs as transitionPs f cls

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = PhyloCTMCPropertiesVariableA substRoot transitionPs cls likelihood alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree) fs nodeCLVs ancestralComponentStates

  return ([likelihood], prop)

instance Dist (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) where
    type Result (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType (Rooted t) ~ Text, HasRoot (Rooted t), HasBranchLengths (Rooted t), RateModel s, IsTree t, SimpleSModel (Rooted t) s, IsTree t2) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) = PhyloCTMCPropertiesVariableA
    annotated_densities (PhyloCTMC tree alignment smodel scale) = annotated_subst_like_on_tree tree alignment (scaleTo scale smodel)

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
  let smodelOnTree = SModelOnTree rtree smodel
      as = pairwise_alignments alignment
      ps = transitionPsMap smodelOnTree
      f = (weightedFrequencyMatrix smodelOnTree)

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence (sequenceLength alignment node) f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateSequenceFrom (stateSequences IntMap.! parent) (as IntMap.! b) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences


instance (HasAlphabet s, IsTree t, HasRoot (Rooted t), LabelType (Rooted t) ~ Text, HasBranchLengths (Rooted t), RateModel s, SimpleSModel (Rooted t) s, IsTree t2) => IOSampleable (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) where
    sampleIO (PhyloCTMC tree alignment rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          rtree = makeRooted tree
          smodel = scaleTo scale rawSmodel
          smap = stateLetters (SModelOnTree rtree smodel)

      stateSequences <- sampleComponentStates rtree alignment smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled rtree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot (Rooted t), LabelType (Rooted t) ~ Text, HasBranchLengths (Rooted t), RateModel s, SimpleSModel (Rooted t) s, IsTree t2) => Sampleable (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) where
    sample dist = RanDistribution2 dist do_nothing


-----------
annotatedSubstLikeOnTreeEqNonRev tree alignment smodel sequenceData = do
  let substRoot = modifiable (head $ internalNodes tree ++ leafNodes tree)

  let n_nodes = numNodes tree
      as = pairwise_alignments alignment
      maybeNodeSequences = labelToNodeMap tree (getSequences sequenceData)
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodelOnTree
      smodelOnTree = SModelOnTree tree smodel
      transitionPs = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree
      fs = getNodesSet tree & IntMap.fromSet (\_ -> f)
      cls = cachedConditionalLikelihoodsEqNonRev tree nodeCLVs as transitionPs f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peelLikelihoodEqNonRev tree nodeCLVs cls as f substRoot

      ancestralComponentStates = sample_ancestral_sequences tree substRoot nodeCLVs as transitionPs f cls

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = PhyloCTMCPropertiesVariableA substRoot transitionPs cls likelihood alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree) fs nodeCLVs ancestralComponentStates

  return ([likelihood], prop)

instance Dist (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumNonReversible) where
    type Result (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumNonReversible) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"


instance (HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s, IsTree t2) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumNonReversible) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumNonReversible) = PhyloCTMCPropertiesVariableA
    annotated_densities (PhyloCTMC tree alignment smodel scale) = annotatedSubstLikeOnTreeEqNonRev tree alignment (scaleTo scale smodel)


instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s, IsTree t2) => IOSampleable (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumNonReversible) where
    sampleIO (PhyloCTMC tree alignment rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          smodel = scaleTo scale rawSmodel
          smap = stateLetters (SModelOnTree tree smodel)

      stateSequences <- sampleComponentStates tree alignment smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s, IsTree t2) => Sampleable (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumNonReversible) where
    sample dist = RanDistribution2 dist do_nothing

---------------------------------------------
annotatedSubstLikeOnTreeNonEq tree alignment smodel sequenceData = do
  let substRoot = modifiable (head $ internalNodes tree ++ leafNodes tree)

  let n_nodes = numNodes tree
      as = pairwise_alignments alignment
      maybeNodeSequences = labelToNodeMap tree (getSequences sequenceData)
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodelOnTree
      smodelOnTree = SModelOnTree tree smodel
      transitionPs = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree
      fs = frequenciesOnTree tree f transitionPs
      cls = cachedConditionalLikelihoodsNonEq tree nodeCLVs as transitionPs f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = peelLikelihoodNonEq tree nodeCLVs cls as f substRoot

      ancestralComponentStates = sample_ancestral_sequences tree substRoot nodeCLVs as transitionPs f cls

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = PhyloCTMCPropertiesVariableA substRoot transitionPs cls likelihood alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree) fs nodeCLVs ancestralComponentStates

  return ([likelihood], prop)

instance Dist (PhyloCTMC t (AlignmentOnTree t2) s NonEquilibrium) where
    type Result (PhyloCTMC t (AlignmentOnTree t2) s NonEquilibrium) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s, IsTree t2) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t2) s NonEquilibrium) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t2) s NonEquilibrium) = PhyloCTMCPropertiesVariableA
    annotated_densities (PhyloCTMC tree alignment smodel scale) = annotatedSubstLikeOnTreeNonEq tree alignment (scaleTo scale smodel)

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s, IsTree t2) => IOSampleable (PhyloCTMC t (AlignmentOnTree t2) s NonEquilibrium) where
    sampleIO (PhyloCTMC tree alignment rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          smodel = scaleTo scale rawSmodel
          smap = stateLetters (SModelOnTree tree smodel)

      stateSequences <- sampleComponentStates tree alignment smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled tree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s, IsTree t2) => Sampleable (PhyloCTMC t (AlignmentOnTree t2) s NonEquilibrium) where
    sample dist = RanDistribution2 dist do_nothing


