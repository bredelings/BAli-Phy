module Probability.Distribution.PhyloCTMC.VariableA.EqNonRev where

import Probability.Distribution.PhyloCTMC.VariableA.Properties
import Probability.Distribution.PhyloCTMC.VariableA.Sample
import Probability.Distribution.PhyloCTMC.PhyloCTMC
import Probability.Random
import Tree
import SModel
import SModel.Likelihood.VariableA
import Bio.Alignment -- for many things.
import Bio.Alphabet (HasAlphabet(..))
import Data.Matrix
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.IntMap as IntMap
import Reversible

annotatedSubstLikeOnTreeEqNonRev tree alignment smodel sequenceData = do
  let substRoot = modifiable (head $ internalNodes tree ++ leafNodes tree)
      rtree = if isReversible smodel
              then setRoot substRoot tree
              else tree

  let as = pairwiseAlignments alignment
      maybeNodeSequences = labelToNodeMap rtree (getSequences sequenceData)
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodelOnTree
      smodelOnTree = SModelOnTree rtree smodel
      transitionPs = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree
      fs = if isStationary smodel
           then getNodesSet rtree & IntMap.fromSet (\_ -> f)
           else frequenciesOnTree rtree f transitionPs
      cls = if isStationary smodel
            then cachedConditionalLikelihoodsEqNonRev rtree nodeCLVs as transitionPs f
            else cachedConditionalLikelihoodsNonEq    rtree nodeCLVs as transitionPs f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = if isStationary smodel
                    then peelLikelihoodEqNonRev rtree nodeCLVs cls as f substRoot
                    else peelLikelihoodNonEq    rtree nodeCLVs cls as f substRoot

      ancestralComponentStates = sampleAncestralSequences rtree substRoot nodeCLVs as transitionPs f cls

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

    
