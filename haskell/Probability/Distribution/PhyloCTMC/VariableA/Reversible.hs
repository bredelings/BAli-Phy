module Probability.Distribution.PhyloCTMC.VariableA.Reversible where

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


{-
 - rtree: translate from nodes to labels
 - alignment: translates from sequences to aligned sequences
 - smap: translates from states to letters
 - alphabet: describes the letters
 -}

annotated_subst_like_on_tree tree alignment smodel sequenceData = do
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
      fs = getNodesSet rtree & IntMap.fromSet (\_ -> f)
      cls = if isStationary smodel
            then cachedConditionalLikelihoodsEqNonRev rtree nodeCLVs as transitionPs f
            else cachedConditionalLikelihoodsNonEq rtree nodeCLVs as transitionPs f
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood  = if isStationary smodel
                    then peelLikelihoodEqNonRev rtree nodeCLVs cls as f substRoot
                    else peelLikelihoodNonEq rtree nodeCLVs cls as f substRoot

      ancestralComponentStates = sampleAncestralSequences rtree substRoot nodeCLVs as transitionPs f cls

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  let prop = PhyloCTMCPropertiesVariableA substRoot transitionPs cls likelihood alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree) fs nodeCLVs ancestralComponentStates

  return ([likelihood], prop)

instance Dist (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) where
    type Result (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) = UnalignedCharacterData
    dist_name _ = "PhyloCTMC"

-- TODO: make this work on forests!                  -
instance (HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s, IsTree t2) => HasAnnotatedPdf (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) where
    type DistProperties (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) = PhyloCTMCPropertiesVariableA
    annotated_densities (PhyloCTMC tree alignment smodel scale) = annotated_subst_like_on_tree tree alignment (scaleTo scale smodel)

-- getSequencesFromTree :: IsGraph t, LabelType t ~ Text => t -> IntMap Sequence ->

-- Uh... how do we get from a [Sequence] and an AlignmentOnTree to a FASTA file?
-- Can we have an IsAlignment class that allows us to get a FASTA from it?
--  + alignmentLength can be implemented for both AlignmentMatrix and AlignmentOnTree
--  + could we change AlignmentOnTree to be data AlignmentOnTree t = OneSequence { tree :: t, length :: Int } | ManySequences { tree :: t, paiwise_alignments :: IntMap PairwiseAlignment} ?
--    one issue is that we can check EITHER the tree OR the alignment constructor to determin of there's only one sequence.
--    We could to AlignmentOnTree { tree :: t, singleLength :: Maybe Int, pairwiseAlignments :: IntMap PairwiseAlignment }
--    In that case, if there's a single length, we still have an (empty) IntMap for the pairwise alignments.

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s, IsTree t2) => IOSampleable (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) where
    sampleIO (PhyloCTMC tree alignment rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          rtree = tree
          smodel = scaleTo scale rawSmodel
          smap = stateLetters (SModelOnTree rtree smodel)

      stateSequences <- sampleComponentStates rtree alignment smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Unaligned $ CharacterData alphabet $ getLabelled rtree sequenceForNode stateSequences

instance (HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s, IsTree t2) => Sampleable (PhyloCTMC t (AlignmentOnTree t2) s EquilibriumReversible) where
    sample dist = RanDistribution2 dist do_nothing


