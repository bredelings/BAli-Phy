module Probability.Distribution.PhyloCTMC.VariableA
    (module Probability.Distribution.PhyloCTMC.Properties,
     module Probability.Distribution.PhyloCTMC.PhyloCTMC,
     module Probability.Distribution.PhyloCTMC.VariableA.Reversible
    )
where

import Probability.Distribution.PhyloCTMC.VariableA.Reversible
import Probability.Distribution.PhyloCTMC.VariableA.Sample
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

-----------
annotatedSubstLikeOnTreeEqNonRev tree alignment smodel sequenceData = do
  let substRoot = modifiable (head $ internalNodes tree ++ leafNodes tree)

  let n_nodes = numNodes tree
      as = pairwiseAlignments alignment
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
      as = pairwiseAlignments alignment
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


