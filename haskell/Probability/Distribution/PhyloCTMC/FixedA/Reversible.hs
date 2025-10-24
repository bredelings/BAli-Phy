module Probability.Distribution.PhyloCTMC.FixedA.Reversible where

import Probability.Distribution.PhyloCTMC.FixedA.Properties
import Probability.Distribution.PhyloCTMC.FixedA.Sample
import Probability.Distribution.PhyloCTMC.PhyloCTMC
import Probability.Random
import Tree
import SModel
import SModel.Likelihood.FixedA
import Bio.Alignment -- for many things.
import Bio.Alphabet (HasAlphabet(..))
import Data.Matrix
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.IntMap as IntMap

annotatedSubstLikelihoodFixedA tree length smodel sequenceData = do
  let substRoot = modifiable (head $ internalNodes rtree ++ leafNodes rtree)
      rtree = if isReversible smodel
              then setRoot substRoot tree
              else tree

  let (isequences, columnCounts, mapping) = compressAlignment $ getSequences sequenceData

      maybeNodeISequences = labelToNodeMap rtree isequences
      maybeNodeSeqsBits = ((\seq -> (stripGaps seq, bitmaskFromSequence seq)) <$>) <$> maybeNodeISequences
      nModels = nrows f
      nodeCLVs = simpleNodeCLVs alphabet smap nModels maybeNodeSeqsBits

      alphabet = getAlphabet smodel
      smap   = stateLetters smodelOnTree
      smodelOnTree = SModelOnTree rtree smodel
      transitionPs = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree
      cls = cachedConditionalLikelihoodsNonRev rtree nodeCLVs transitionPs f
      likelihood = peelLikelihoodNonRev nodeCLVs rtree cls f alphabet smap substRoot columnCounts

      ancestralComponentStates = sampleAncestralSequences tree substRoot nodeCLVs alphabet transitionPs f cls smap mapping

  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  let prop = PhyloCTMCPropertiesFixedA substRoot transitionPs cls likelihood alphabet (SModel.nStates smodelOnTree) (SModel.nBaseModels smodelOnTree) ancestralComponentStates

  return ([likelihood], prop)

instance Dist (PhyloCTMC t Int s) where
    type Result (PhyloCTMC t Int s) = AlignedCharacterData
    dist_name _ = "PhyloCTMCFixedA"

-- TODO: make this work on forests!
instance (CheckReversible s, HasAlphabet s, LabelType t ~ Text, HasRoot t, HasBranchLengths t, RateModel s, IsTree t, SimpleSModel t s) => HasAnnotatedPdf (PhyloCTMC t Int s) where
    type DistProperties (PhyloCTMC t Int s) = PhyloCTMCPropertiesFixedA
    annotated_densities (PhyloCTMC tree length smodel scale) = annotatedSubstLikelihoodFixedA tree length (scaleTo scale smodel)

instance (HasAlphabet s, HasRoot t, LabelType t ~ Text, HasBranchLengths t, RateModel s, SimpleSModel t s) => IOSampleable (PhyloCTMC t Int s) where
    sampleIO (PhyloCTMC rtree rootLength rawSmodel scale) = do
      let alphabet = getAlphabet smodel
          smodel = scaleTo scale rawSmodel
          smap = stateLetters (SModelOnTree rtree smodel)

      stateSequences <- sampleComponentStatesFixed rtree rootLength smodel

      let sequenceForNode label stateSequence = (label, statesToLetters smap $ extractStates stateSequence)

      return $ Aligned $ CharacterData alphabet $ getLabelled rtree sequenceForNode stateSequences

instance (CheckReversible s, HasAlphabet s, IsTree t, HasRoot t, LabelType t ~ Text, HasBranchLengths t, HasBranchLengths t, RateModel s, SimpleSModel t s) => Sampleable (PhyloCTMC t Int s) where
    sample dist = RanDistribution2 dist do_nothing




